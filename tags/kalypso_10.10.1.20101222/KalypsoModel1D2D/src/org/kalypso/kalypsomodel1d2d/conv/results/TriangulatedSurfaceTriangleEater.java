/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.io.File;
import java.util.Date;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.TinResultWriter.QNameAndString;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;

/**
 * @author Thomas Jung
 */
public class TriangulatedSurfaceTriangleEater implements ITriangleEater
{
  private final GM_TriangulatedSurface m_surface;

  private ResultType.TYPE m_parameter = null;

  private GMLWorkspace m_workspace = null;

  private File m_tinResultFile = null;

  public TriangulatedSurfaceTriangleEater( final String crs, final QNameAndString[] properties ) throws GM_Exception
  {
    this( null, null, new GM_TriangulatedSurface_Impl( crs ), null, properties );
  }

  public TriangulatedSurfaceTriangleEater( final File tinResultFile, final GMLWorkspace workspace, final GM_TriangulatedSurface surface, final ResultType.TYPE parameter, final QNameAndString[] properties )
  {
    m_surface = surface;
    m_parameter = parameter;
    m_workspace = workspace;
    m_tinResultFile = tinResultFile;

    if( m_workspace != null )
    {
      final Feature rootFeature = m_workspace.getRootFeature();
      for( final QNameAndString nameAndString : properties )
        rootFeature.setProperty( nameAndString.m_qname, nameAndString.m_value );
    }
  }

  /**
   * add a triangle to the eater. The triangle is defined by its three nodes ({@link INodeResult} and a information, if
   * the triangle is marked as wet or dry.
   * 
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#add(java.util.List)
   */
  @Override
  public void add( final INodeResult... nodes )
  {
    try
    {
      final GM_Triangle_Impl gmTriangle = TriangulatedSurfaceDirectTriangleEater.createTriangle( nodes, m_parameter );
      if( gmTriangle != null )
        m_surface.add( gmTriangle );
    }
    catch( final GM_Exception e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#finished()
   */
  @Override
  public void finished( )
  {
    if( m_surface.size() == 0 || m_tinResultFile == null || m_workspace == null )
      return;

    final String name = m_tinResultFile.getPath();

    final int extensionIndex = name.lastIndexOf( "." ); //$NON-NLS-1$

    final String substring = name.substring( 0, extensionIndex );
    final String extension = name.substring( extensionIndex, name.length() );

    /* create filename */
    String param;
    if( m_parameter != null )
      param = m_parameter.name();
    else
      param = ""; //$NON-NLS-1$

    final String fileName = substring + "_" + param + extension; //$NON-NLS-1$

    try
    {
      // TODO: zip Url + stream
      final File paramFile = new File( fileName );
      //      if( extension.equals( ".zip" ) ) //$NON-NLS-1$
      // {
      //                GmlSerializer.serializeWorkspaceToZipFile( paramFile, m_workspace, "tin_" + param + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      // }
      // else
      // {
      GmlSerializer.serializeWorkspace( paramFile, m_workspace, "UTF-8" ); //$NON-NLS-1$
      // }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater.10" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#setTime(java.util.Date)
   */
  @Override
  public void setTime( final Date date )
  {
    if( m_workspace == null )
      return;

    final Feature triangleFeature = m_workspace.getRootFeature();
    if( triangleFeature != null )
    {
      final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( date );
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "date" ), gregorianCalendar ); //$NON-NLS-1$
    }
  }

  public void addPoints( final List<GM_Point> pointList )
  {
    final String crs = pointList.get( 0 ).getCoordinateSystem();

    GM_Triangle_Impl gmTriangle = null;

    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < pointList.size(); i++ )
    {
      final GM_Point point = pointList.get( i );
      pos[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( point.getX(), point.getY(), point.getZ() );
    }

    try
    {
      // for shape export we need a clockwise orientation, the algorithm that delivers the nodes is the nodes to the
      // eater is thinking counter-clockwise.
      // for that reason we switch the positions order to get that clockwise orientation.
      gmTriangle = new GM_Triangle_Impl( pos[2], pos[1], pos[0], crs );
    }
    catch( final GM_Exception e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater.13" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
    }

    if( gmTriangle != null )
      m_surface.add( gmTriangle );
  }

  public GM_TriangulatedSurface getSurface( )
  {
    return m_surface;
  }
}
