package org.kalypsodeegree_impl.model.feature.xpath;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Der FeaturePath denotiert ein Feature, eine FeatureList oder einen IFeatureType innerhalb eines GMLWorkspace.
 * Notation: <br>
 * property ::= Der Name einer FeatureAssociationProperty <br>
 * typename ::= Ein beliebiger Typname, sollte nur für abgeleitete Typen benutzt werden <br>
 * emptypath ::= Der leere Pfad, zeigt auf das Root-Feature bzw. dessen Typ segment ::= #fid# <id>| <property>|
 * <property>[ <typename>] featurePath ::= <emptypath>| <segment>/ <segment>]]>
 * 
 * @author doemming
 */
public class FeatureXPath
{
  /** Separates two segments in the feature-path */
  public final static char SEGMENT_SEPARATOR = '/';

  public final static char TYPENAME_TAG_OPEN = '[';

  public final static char TYPENAME_TAG_CLOSE = ']';

  /** Something between two '/' */
  private final Segment[] m_segments;

  public FeatureXPath( final String path )
  {
    if( path.trim().length() == 0 )
      m_segments = new Segment[] {};
    else
    {
      final String[] segments = path.split( "/" );
      m_segments = new Segment[segments.length];
      for( int i = 0; i < segments.length; i++ )
        m_segments[i] = new Segment( segments[i] );
    }
  }

  public FeatureXPath( final Feature feature )
  {
    final String id = feature.getId();
    if( id == null || id.length() < 1 )
      m_segments = new Segment[0];
    else
      m_segments = new Segment[] { new Segment( feature ) };
  }

  public FeatureXPath( final FeatureXPath parent, final String segment )
  {
    final int parentLength = parent.m_segments.length;
    m_segments = new Segment[parentLength + 1];
    System.arraycopy( parent.m_segments, 0, m_segments, 0, parentLength );
    m_segments[parentLength] = new Segment( segment );
  }

  /**
   * <p>
   * Gibt das durch den FeaturPath gegebene Feature zurück.
   * </p>
   * <p>
   * Syntax des FeaturePath: <code> <propertyName>/.../<propertyName>[featureTypeName] </code> Wobei der
   * featureTypeName optional ist
   * </p>
   * <p>
   * Es darf innerhalb des Pfads keine (Feature)Liste vorkommen, nur am Ende
   * </p>
   * <p>
   * Ist der Typ-Name angegeben und wurde eine Liste gefunden, wird eine (neue) FeatureList zurückgegeben, deren
   * Elemente alle vom angegebenen Typ sind
   * </p>
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureFromPath(java.lang.String)
   */
  public Object getFeature( final GMLWorkspace workspace ) throws FeaturePathException
  {
    return getFeatureForSegment( workspace, workspace.getRootFeature(), 0 );
  }

  public Object getFeatureForSegment( final GMLWorkspace workspace, final Feature feature, final int segmentIndex ) throws FeaturePathException
  {
    if( segmentIndex >= m_segments.length )
      return feature;

    final Object value = m_segments[segmentIndex].getValue( workspace, feature );
    if( value instanceof Feature )
      return getFeatureForSegment( workspace, (Feature) value, segmentIndex + 1 );
    else if( value instanceof String )
      return getFeatureForSegment( workspace, workspace.getFeature( (String) value ), segmentIndex + 1 );
    else if( value instanceof FeatureList && segmentIndex == m_segments.length - 1 )
      return value;
    // TODO
    // alles andere ist ein Fehler
    return null;
  }

//  /** Voraussetzung, mindestens das Root-Feature muss existieren */
//  public IFeatureType getFeatureType( final GMLWorkspace workspace )
//  {
//    final IFeatureType rootType = workspace.getRootFeature().getFeatureType();
//    return getFeatureTypeForSegment( workspace, rootType, 0 );
//  }

//  private IFeatureType getFeatureTypeForSegment( final GMLWorkspace workspace, final IFeatureType featureType, final int segmentIndex )
//  {
//    if( segmentIndex >= m_segments.length )
//      return featureType;
//
//    final Segment segment = m_segments[segmentIndex];
//    final IFeatureType type = segment.getType( workspace, featureType );
//
//    return getFeatureTypeForSegment( workspace, type, segmentIndex + 1 );
//  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buffer = new StringBuffer();

    for( int i = 0; i < m_segments.length; i++ )
    {
      buffer.append( m_segments[i].toString() );
      if( i != m_segments.length - 1 )
        buffer.append( SEGMENT_SEPARATOR );
    }

    return buffer.toString();
  }
}
