/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 * 
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.sort.FilteredFeatureList;

/**
 * Der FeaturePath denotiert ein Feature, eine FeatureList oder einen IFeatureType innerhalb eines GMLWorkspace.
 * Notation: <br>
 * property ::= Der Name einer FeatureAssociationProperty <br>
 * typename ::= Ein beliebiger Typname, sollte nur für abgeleitete Typen benutzt werden <br>
 * emptypath ::= Der leere Pfad, zeigt auf das Root-Feature bzw. dessen Typ segment ::= #fid# <id>| <property>|
 * <property>[ <typename>] featurePath ::= <emptypath>| <segment>/ <segment>]]>
 * 
 * @author belger
 */
public class FeaturePath
{
  /** Separates two segments in the feature-path */
  public final static char SEGMENT_SEPARATOR = '/';

  public final static char TYPENAME_TAG_OPEN = '[';

  public final static char TYPENAME_TAG_CLOSE = ']';

  /** Something between two '/' */
  private final Segment[] m_segments;

  public FeaturePath( final String path )
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

  public FeaturePath( final Feature feature )
  {
    final String id = feature.getId();
    if( id == null || id.length() < 1 )
    {
      // TODO: this leads to subtle bugs
      // better throw an exception here?
      System.out.println( "Feature has no id: " + feature );
      m_segments = new Segment[0];
    }
    else
      m_segments = new Segment[] { new Segment( feature ) };
  }

  public FeaturePath( final FeaturePath parent, final String segment )
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
  public Object getFeature( final GMLWorkspace workspace )
  {
    return getFeatureForSegment( workspace, workspace.getRootFeature(), 0 );
  }

  public Object getFeatureForSegment( final GMLWorkspace workspace, final Feature feature, final int segmentIndex )
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
    else if( value instanceof FeatureList && segmentIndex < m_segments.length - 1 )
    {
      final Feature subFeature = (Feature) ((FeatureList) value).get( 0 );
      return getFeatureForSegment( workspace, subFeature, segmentIndex + 1 );
    }

    return value;
  }

  /** Voraussetzung, mindestens das Root-Feature muss existieren */
  public IFeatureType getFeatureType( final GMLWorkspace workspace )
  {
    final IFeatureType rootType = workspace.getRootFeature().getFeatureType();
    return getFeatureTypeForSegment( workspace, rootType, 0 );
  }

  private IFeatureType getFeatureTypeForSegment( final GMLWorkspace workspace, final IFeatureType featureType, final int segmentIndex )
  {
    if( segmentIndex >= m_segments.length )
      return featureType;

    final Segment segment = m_segments[segmentIndex];
    final IFeatureType type = segment.getType( workspace, featureType );

    return getFeatureTypeForSegment( workspace, type, segmentIndex + 1 );
  }

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

  private final class Segment
  {
    private final static String ID_MARKER = "#fid#";

    private final String m_name;

    private final boolean m_isId;

    /** Path may be filtered with type (Applies only to End of Path) */
    private final String m_typename;

    public Segment( final String segment )
    {
      m_isId = segment.startsWith( ID_MARKER );

      // FeatureID?: '#fid#pegel_123'
      if( m_isId )
      {
        m_name = segment.substring( ID_MARKER.length() );
        m_typename = null;
      }
      else
      {
        if( segment.endsWith( "]" ) )
        {
          final int start = segment.lastIndexOf( '[' );
          m_typename = segment.substring( start + 1, segment.length() - 1 );
          m_name = segment.substring( 0, start );
        }
        else
        {
          m_typename = null;
          m_name = segment;
        }
      }
    }

    public Segment( final Feature feature )
    {
      m_name = feature.getId();
      m_typename = null;
      m_isId = true;
    }

    public final String getName( )
    {
      return m_name;
    }

    public boolean isID( )
    {
      return m_isId;
    }

    public Object getValue( final GMLWorkspace workspace, final Feature feature )
    {
      final String name = getName();

      if( isID() )
        return workspace.getFeature( name );

      final Object value = feature.getProperty( name );

      // falls ein bestimmter typ gewünscht ist, jetzt filtern
      // geht natürlich nur bei FeatureListen
      if( m_typename != null )
      {
        if( value instanceof FeatureList )
          return new FilteredFeatureList( (FeatureList) value, m_typename, true );

        return null;
      }

      return value;
    }

    public IFeatureType getType( final GMLWorkspace workspace, final IFeatureType featureType )
    {
      if( isID() )
        return workspace.getFeature( getName() ).getFeatureType();

      final IPropertyType ftp = featureType.getProperty( getName() );
      if( ftp instanceof IRelationType )
      {
        final IRelationType relationPT = (IRelationType) ftp;
        if( m_typename != null )
        {
          final IFeatureType associationFeatureType = relationPT.getTargetFeatureType();
          final IGMLSchema contexstSchema = workspace.getGMLSchema();
          final IFeatureType[] associationFeatureTypes = GMLSchemaUtilities.getSubstituts( associationFeatureType, contexstSchema, false, true );
          for( final IFeatureType type : associationFeatureTypes )
          {
            final IFeatureType[] substituts = GMLSchemaUtilities.getSubstituts( type, workspace.getGMLSchema(), true, true );
            for( final IFeatureType substType : substituts )
            {
              if( m_typename.equals( substType.getName() ) )
                return substType;
            }

            // if( m_typename.equals( type.getName() ) )
            // return type;
          }

          return null;
        }

        return relationPT.getTargetFeatureType();
      }
      return null;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString( )
    {
      final StringBuffer buffer = new StringBuffer();

      if( isID() )
        buffer.append( ID_MARKER );

      buffer.append( m_name );

      if( m_typename != null )
        buffer.append( '[' ).append( m_typename ).append( ']' );

      return buffer.toString();
    }
  }
}
