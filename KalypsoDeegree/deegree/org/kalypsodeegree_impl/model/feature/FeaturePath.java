package org.kalypsodeegree_impl.model.feature;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.sort.FilteredFeatureList;

/**
 * @author belger
 */
public class FeaturePath
{
  /** Separates two segments in the feature-path */
  public final static char SEGMENT_SEPARATOR = '/';
  
  /** Somethin gbetween two '/' */
  private final Segment[] m_segments;
  
  /** Path may be filtered with type (Applies only to End of Path) */
  private final String m_typename;
  
  public FeaturePath( final String path )
  {
    if( path.trim().length() == 0 )
    {
      m_segments = new Segment[] {};
      m_typename = null;
    }
    else
    {
      final String realPath;
      if( path.endsWith( "]" ) )
      {
        final int start = path.lastIndexOf( '[' );
        m_typename = path.substring( start + 1, path.length() - 1 );
        realPath = path.substring( 0, start );
      }
      else
      {
        realPath = path;
        m_typename = null;
      }
      
      final String[] segments = realPath.split( "/" );
      m_segments = new Segment[segments.length];
      for( int i = 0; i < segments.length; i++ )
        m_segments[i] = new Segment( segments[i] );
    }
  }
  
  public FeaturePath( final Feature feature )
  {
    m_segments = new Segment[] { new Segment( feature ) };
    m_typename = null;
  }
  
  public FeaturePath( final FeaturePath parent, final String segment )
  {
    final int parentLength = parent.m_segments.length;
    m_segments = new Segment[parentLength + 1];
    System.arraycopy( parent.m_segments, 0, m_segments, 0, parentLength );
    m_segments[parentLength] = new Segment( segment );

    // wenn der Parent einen Typnamen hat, macht das ganze eigentlich keinen sinn
    m_typename = parent.m_typename;
  }

  /**
   * <p>
   * Gibt das durch den FeaturPath gegebene Feature zurück.
   * </p>
   * <p>
   * Syntax des FeaturePath:
   * <code> <propertyName>/.../<propertyName>[featureTypeName] </code> Wobei
   * der featureTypeName optional ist
   * </p>
   * <p>
   * Es darf innerhalb des Pfads keine (Feature)Liste vorkommen, nur am Ende
   * </p>
   * <p>
   * Ist der Typ-Name angegeben und wurde eine Liste gefunden, wird eine (neue)
   * FeatureList zurückgegeben, deren Elemente alle vom angegebenen Typ sind
   * </p>
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureFromPath(java.lang.String)
   */
  public Object getFeature( final GMLWorkspace workspace )
  {
    final Object value = getFeatureForSegment( workspace, workspace.getRootFeature(), 0 );
    // falls ein bestimmter typ gewünscht ist, jetzt filtern
    // geht natürlich nur bei FeatureListen
    if( m_typename != null )
    {
      if( value instanceof FeatureList )
        return  new FilteredFeatureList( (FeatureList)value, m_typename, true );
      
      return null;
    }
    
    return value;
  }
  
  public Object getFeatureForSegment( final GMLWorkspace workspace, final Feature feature, final int segmentIndex )
  {
    if( segmentIndex >= m_segments.length )
      return feature;

    final Object value = m_segments[segmentIndex].getValueForSegment( workspace, feature );
    if( value instanceof Feature )
      return getFeatureForSegment( workspace, (Feature)value, segmentIndex + 1 );
    else if( value instanceof String )
      return getFeatureForSegment( workspace, workspace.getFeature( (String)value ), segmentIndex + 1 );
    else if( value instanceof FeatureList && segmentIndex == m_segments.length - 1 )
      return value;

    // alles andere ist ein Fehler
    return null;
  }
  
  /** Voraussetzung, mindestens das Root-Feature muss existieren */
  public FeatureType getFeatureType( final GMLWorkspace workspace )
  {
    if( m_typename != null )
      workspace.getFeatureType( m_typename );
    
    final FeatureType featureType = workspace.getRootFeature().getFeatureType();
    return getFeatureTypeForSegment( workspace, featureType, 0 );
  }
  
  private FeatureType getFeatureTypeForSegment( final GMLWorkspace workspace, final FeatureType featureType, final int segmentIndex )
  {
    if( segmentIndex >= m_segments.length )
      return featureType;

    final Segment segment = m_segments[segmentIndex];
    final FeatureType type = segment.getTypeForSegment( workspace, featureType );

    return getFeatureTypeForSegment( workspace, type, segmentIndex + 1 );
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final StringBuffer buffer = new StringBuffer();
    
    for( int i = 0; i < m_segments.length; i++ )
    {
      buffer.append( m_segments[i].toString() );
      if( i != m_segments.length - 1 )
        buffer.append( SEGMENT_SEPARATOR );
    }
    
    if( m_typename != null )
      buffer.append( '[' ).append( m_typename ).append( ']' );
    
    return buffer.toString();
  }
  
  private final class Segment
  {
    private final static String ID_MARKER = "#fid#";

    private final String m_name;
    private final boolean m_isId;

    public Segment( final String segment )
    {
      m_isId = segment.startsWith( ID_MARKER );
      
      // FeatureID?: '#fid#pegel_123'
      if( m_isId )
        m_name = segment.substring( ID_MARKER.length() );
      else
        m_name = segment;
    }
    
    public Segment( final Feature feature )
    {
      m_name = feature.getId();
      m_isId = true;
    }

    public final String getName()
    {
      return m_name;
    }
    
    public boolean isID()
    {
      return m_isId;
    }
    
    public Object getValueForSegment( final GMLWorkspace workspace, final Feature feature )
    {
      if( isID() )
        return workspace.getFeature( getName() );

      return feature.getProperty( getName() );
    }

    public FeatureType getTypeForSegment( final GMLWorkspace workspace, final FeatureType featureType )
    {
      if( isID() )
        return workspace.getFeature( getName() ).getFeatureType();
      
      final FeatureTypeProperty ftp = featureType.getProperty( getName() );
      if( ftp instanceof FeatureAssociationTypeProperty )
        return ((FeatureAssociationTypeProperty)ftp).getAssociationFeatureType();
      
      return null;
    }
    
    /**
     * @see java.lang.Object#toString()
     */
    public String toString()
    {
      if( isID() )
        return ID_MARKER + m_name;
      
      return m_name;
    }
  }
}
