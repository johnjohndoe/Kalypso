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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Calendar;
import java.util.Date;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.xml.bind.DatatypeConverter;
import javax.xml.namespace.QName;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.TinResultWriter.QNameAndString;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This eater writes the triangles into an GML-File as TriangualtedSurface.<br>
 * The triangles directly get written without storing them into an intermediate GML-Workspace.
 * 
 * @author Thomas Jung
 */
public class TriangulatedSurfaceDirectTriangleEater implements ITriangleEater
{
  private final ResultType m_parameter;

  private final File m_tinResultFile;

  private final TinResultWriter m_writer;

  private final String m_crs;

  private ZipEntry m_zipEntry;

  private OutputStream m_os;

  public TriangulatedSurfaceDirectTriangleEater( final File tinResultFile, final ResultType parameter, final String crs, final QNameAndString[] props ) throws CoreException
  {
    m_parameter = parameter;
    m_tinResultFile = tinResultFile;
    m_crs = crs;
    m_writer = initMarshaller( props );
  }

  /**
   * add a triangle to the eater. The triangle is defined by its three nodes ({@link INodeResult} and a information, if
   * the triangle is marked as wet or dry.
   */
  @Override
  public void add( final INodeResult... nodes )
  {
    if( m_writer == null )
      return;

    if( nodes.length < 3 )
      return;

    try
    {
      final GM_Position[] triangle = processNodes( nodes, m_parameter );
      if( triangle != null )
        m_writer.add( triangle );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater.1" ), e ); //$NON-NLS-1$
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    }
  }

  public static GM_Triangle createTriangle( final INodeResult[] nodes, final ResultType parameter )
  {
    if( nodes.length < 3 )
      return null;

    final GM_Position pos[] = processNodes( nodes, parameter );
    if( pos != null )
    {
      final String crs = nodes[0].getPoint().getCoordinateSystem();
      return GeometryFactory.createGM_Triangle( pos[0], pos[1], pos[2], crs );
    }

    return null;
  }

  private static GM_Position[] processNodes( final INodeResult[] nodes, final ResultType parameter )
  {
    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < nodes.length; i++ )
    {
      final INodeResult nodeResult = nodes[i];
      final GM_Point point = nodeResult.getPoint();
      final double x = point.getX();
      final double y = point.getY();
      final double z = getZValue( nodeResult, parameter );

      // TODO: make this configurable
      // exclude triangles with water levels below terrain surface
      if( parameter == ResultType.WATERLEVEL && z < point.getZ() )
        return null;

      pos[i] = GeometryFactory.createGM_Position( x, y, z );
    }
    return pos;
  }

  private static double getZValue( final INodeResult nodeResult, final ResultType parameter )
  {
    switch( parameter )
    {
      case VELOCITY:
        return nodeResult.getAbsoluteVelocity();

      case VELOCITY_X:
        return nodeResult.getVelocity().get( 0 );

      case VELOCITY_Y:
        return nodeResult.getVelocity().get( 1 );

      case WATERLEVEL:
        return nodeResult.getWaterlevel();

      case DEPTH:
        return nodeResult.getDepth();

      case TERRAIN:
        return nodeResult.getPoint().getZ();

      case SHEARSTRESS:

        // get the node shear stress depending on nodes velocity and
        // the averaged lambda value (derived by the neighboring elements).
        final double lambda = nodeResult.getAveragedLambda();

        // get velocity
        final double vAbs = nodeResult.getAbsoluteVelocity();
        // calculate shearstress
        return lambda * 0.125 * 1000 * vAbs * vAbs;

      default:
        return nodeResult.getPoint().getZ();
    }

  }

  @Override
  public void finished( ) throws CoreException
  {
    try
    {
      m_writer.finished();

      if( m_os instanceof ZipOutputStream )
        ((ZipOutputStream)m_os).closeEntry();
      m_os.close();
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater.0" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( m_os );
    }
  }

  @Override
  public void setTime( final Date date )
  {
    final Calendar calendar = Calendar.getInstance();
    calendar.setTime( date );
    final String printedDateTime = DatatypeConverter.printDateTime( calendar );

    m_writer.addProperty( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "date" ), printedDateTime ) );//$NON-NLS-1$
  }

  /**
   * Returns the marshaller used to write the triangle, initialises it (and hence opens the file), if not yet done
   */
  private TinResultWriter initMarshaller( final QNameAndString[] props ) throws CoreException
  {
    try
    {
      final String tinFilePath = m_tinResultFile.getPath();
      final String tinFileBase = FileUtilities.nameWithoutExtension( tinFilePath ) + "_" + m_parameter.name(); //$NON-NLS-1$

      final String extension = FilenameUtils.getExtension( tinFilePath ).toLowerCase();

      if( "zip".equals( extension ) ) //$NON-NLS-1$
      {
        m_os = new ZipOutputStream( new FileOutputStream( tinFileBase + ".zip" ) ); //$NON-NLS-1$
        m_zipEntry = new ZipEntry( "tin_" + m_parameter.name() + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
        ((ZipOutputStream)m_os).putNextEntry( m_zipEntry );
      }
      else if( "gz".equals( extension ) ) //$NON-NLS-1$
        m_os = new GZIPOutputStream( new BufferedOutputStream( new FileOutputStream( new File( tinFileBase + ".gz" ) ) ) ); //$NON-NLS-1$
      else
        m_os = new BufferedOutputStream( new FileOutputStream( new File( tinFileBase + ".gml" ) ) ); //$NON-NLS-1$

      return new TinResultWriter( m_os, m_crs, props );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater.1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

}
