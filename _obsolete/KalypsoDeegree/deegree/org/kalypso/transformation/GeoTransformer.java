/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de


 history:

 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always.

 If you intend to use this software in other ways than in kalypso
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree,
 original copyright:

 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.transformation;

import org.deegree.crs.transformations.CRSTransformation;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Class for transforming deegree geometries to new coordinate reference systems.
 * 
 * @author Holger Albert
 */
public class GeoTransformer
{
  /**
   * The name of the target coordinate system.
   */
  private String m_target;

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param target
   *            The name of the target coordinate system.
   */
  public GeoTransformer( String target )
  {
    m_target = target;

  }

  /**
   * This function returns the name of the target coordinate system.
   * 
   * @return The name of the target coordinate system.
   */
  public String getTarget( )
  {
    return m_target;
  }

  /**
   * This function transforms the coordinates of a deegree geometry to the target coordinate reference system.
   * 
   * @param geo
   *            The object to be transformed.
   * @return The transformed object.
   */
  public GM_Object transform( GM_Object geo ) throws Exception
  {
    if( geo == null )
      return null;

    String cs = geo.getCoordinateSystem();
    if( cs == null || cs.equalsIgnoreCase( m_target ) )
      return geo;

    return geo.transform( getTransformation( cs ), m_target );
  }

  /**
   * This function returns a transformation for a source coordinate system name.<br>
   * Therefore it uses the {@link CachedTransformationFactory}.
   * 
   * @param source
   *            The name of the source coordinate system.
   * @return The transformation from the source coordinate system to the GeoTransformers target coordinate system.
   */
  private CRSTransformation getTransformation( String source ) throws Exception
  {
    CachedTransformationFactory transformationFactory = CachedTransformationFactory.getInstance();
    CRSTransformation transformation = transformationFactory.createFromCoordinateSystems( source, m_target );

    return transformation;
  }

  /**
   * This function transforms a <tt>GM_Envelope</tt> to the target coordinate system of the <tt>GeoTransformer</tt>
   * instance.
   * 
   * @param envelope
   *            The envelope to be transformed.
   * @param source
   *            The name of the source coordinate system.
   * @return The transformed envelope.
   */
  public GM_Envelope transformEnvelope( GM_Envelope envelope, String source ) throws Exception
  {
    if( envelope == null )
      return null;

    if( source == null || source.equalsIgnoreCase( m_target ) )
      return envelope;

    // TODO: this can be improved....
    GM_Surface< ? > asSurface = GeometryFactory.createGM_Surface( envelope, source );
    return transform( asSurface ).getEnvelope();
  }
  
  /**
   * This function transforms the coordinates of a deegree position to the target coordinate reference system.
   * 
   * @param position
   *            The position to be transformed.
   * @return The transformed position.
   */
  public GM_Position transformPosition( GM_Position position, String source ) throws Exception
  {
    if( position == null )
      return null;

    if( source == null || source.equalsIgnoreCase( m_target ) )
      return position;

    return position.transform( getTransformation( source ));
  }
}