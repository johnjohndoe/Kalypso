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
package org.kalypsodeegree_impl.model.ct;

import org.kalypso.transformation.TransformUtilities;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * class for transforming deegree geometries to new coordinate reference systems.
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
final public class GeoTransformer
{
  private static final ConvenienceCSFactory m_csFactory = ConvenienceCSFactory.getInstance();

  private CS_CoordinateSystem m_targetOGCCS = null;

  private CoordinateSystem m_targetCS = null;

  private final boolean m_shouldTransform;

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param targetCS
   * @throws Exception
   */
  public GeoTransformer( final String targetCS ) throws Exception
  {
    this( targetCS, TransformUtilities.shouldTransform() );
  }

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param targetCS
   * @throws Exception
   */
  public GeoTransformer( final String targetCS, final boolean shouldTransform ) throws Exception
  {
    this( m_csFactory.getCSByName( targetCS ), shouldTransform );
  }

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param targetCS
   * @throws Exception
   */
  public GeoTransformer( final CoordinateSystem targetCS ) throws Exception
  {
    this( targetCS, TransformUtilities.shouldTransform() );
  }

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param targetCS
   * @throws Exception
   */
  public GeoTransformer( final CoordinateSystem targetCS, final boolean shouldTransform ) throws Exception
  {
    setTargetCS( targetCS );
    m_shouldTransform = shouldTransform;
  }

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param targetCS
   * @throws Exception
   */
  public GeoTransformer( final CS_CoordinateSystem targetCS ) throws Exception
  {
    this( targetCS, TransformUtilities.shouldTransform() );
  }

  /**
   * Creates a new GeoTransformer object.
   * 
   * @param targetCS
   * @throws Exception
   */
  public GeoTransformer( final CS_CoordinateSystem targetCS, final boolean shouldTransform ) throws Exception
  {
    setTargetCS( targetCS );
    m_shouldTransform = shouldTransform;
  }

  /**
   * sets the target coordinate reference system of the Transformer
   */
  public void setTargetCS( final CS_CoordinateSystem targetCS ) throws Exception
  {
    m_targetOGCCS = targetCS;
    m_targetCS = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().wrap( targetCS );
  }

  /**
   * sets the target coordinate reference system of the Transformer
   */
  public void setTargetCS( final CoordinateSystem targetCS ) throws Exception
  {
    m_targetCS = targetCS;
    m_targetOGCCS = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( targetCS );
  }

  /**
   * returns the target CRS of the <tt>GeoTransformer</tt>
   */
  public CS_CoordinateSystem getTargetCS( )
  {
    return m_targetOGCCS;
  }

  /**
   * transforms the coodinates of a deegree geometry to the target coordinate reference system.
   */
  public GM_Object transform( GM_Object geo ) throws Exception
  {
    if( !m_shouldTransform )
      return geo;

    final CoordinateSystem cs = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().wrap( geo.getCoordinateSystem() );

    if( cs == null || cs.equals( m_targetCS ) )
      return geo;

    final ConvenienceTransformFactory ctf = ConvenienceTransformFactory.getInstance();
    final MathTransform trans = ctf.getTransform( cs, m_targetCS );

    return geo.transform( trans, m_targetOGCCS );

  }

  /**
   * transfroms a <tt>GM_Envelope</tt> to the target crs of the <tt>GeoTransformer</tt> instance
   * 
   * @param envelope
   * @param sourceCRS
   *            CRS of the envelope
   * @throws Exception
   */
  public GM_Envelope transformEnvelope( GM_Envelope envelope, final String sourceCRS ) throws Exception
  {
    Debug.debugMethodBegin( this, "transformPositions" );

    final CoordinateSystem cs = m_csFactory.getCSByName( sourceCRS );

    envelope = transformEnvelope( envelope, cs );

    Debug.debugMethodEnd();
    return envelope;
  }

  /**
   * transfroms a <tt>GM_Envelope</tt> to the target crs of the <tt>GeoTransformer</tt> instance
   * 
   * @param envelope
   * @param sourceCRS
   *            CRS of the envelope
   * @throws Exception
   */
  public GM_Envelope transformEnvelope( GM_Envelope envelope, final CoordinateSystem sourceCRS ) throws Exception
  {
    Debug.debugMethodBegin( this, "transformPositions" );

    final CS_CoordinateSystem cs = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( sourceCRS );
    envelope = transformEnvelope( envelope, cs );

    Debug.debugMethodEnd();
    return envelope;
  }

  /**
   * transfroms a <tt>GM_Envelope</tt> to the target crs of the <tt>GeoTransformer</tt> instance
   * 
   * @param envelope
   * @param sourceCRS
   *            CRS of the envelope
   * @throws Exception
   */
  public GM_Envelope transformEnvelope( final GM_Envelope envelope, final CS_CoordinateSystem sourceCRS ) throws Exception
  {
    /* If no transformation should be done, return the input object. */
    if( !m_shouldTransform )
      return envelope;

    // Debug.debugMethodBegin( this, "transformPositions" );
    //
    final GM_Surface asSurface = GeometryFactory.createGM_Surface( envelope, sourceCRS );
    return transform( asSurface ).getEnvelope();
  }
}