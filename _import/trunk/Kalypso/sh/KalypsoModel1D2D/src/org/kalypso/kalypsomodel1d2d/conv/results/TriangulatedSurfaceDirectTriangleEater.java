/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.xml.bind.DatatypeConverter;
import javax.xml.namespace.QName;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater.QNameAndString;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.transformation.CRSHelper;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.io.sax.TriangulatedSurfaceMarshaller;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.helpers.XMLReaderFactory;

import com.sun.org.apache.xml.internal.serializer.ToXMLStream;

/**
 * This eater writes the triangles into an GML-File as TriangualtedSurface.<br>
 * The triangles directly get written without storing them into an intermediate GML-Workspace.
 * 
 * @author Thomas Jung
 */
public class TriangulatedSurfaceDirectTriangleEater implements ITriangleEater
{
  private final ResultType.TYPE m_parameter;

  private final File m_tinResultFile;

  private TriangulatedSurfaceMarshaller m_marshaller;

  private ToXMLStream m_xmlStream;

  private final String m_crs;

  private final List<QNameAndString> m_props = new ArrayList<QNameAndString>();

  private ZipEntry m_zipEntry;

  public TriangulatedSurfaceDirectTriangleEater( final File tinResultFile, final ResultType.TYPE parameter, final String crs, final QNameAndString[] props ) throws CoreException
  {
    m_parameter = parameter;
    m_tinResultFile = tinResultFile;
    m_crs = crs;
    m_props.addAll( Arrays.asList( props ) );
    m_marshaller = initMarshaller();
  }

  /**
   * add a triangle to the eater. The triangle is defined by its three nodes ({@link INodeResult} and a information, if
   * the triangle is marked as wet or dry.
   * 
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#add(java.util.List)
   */
  public void add( final INodeResult... nodes )
  {
    if( m_marshaller == null )
      return;

    try
    {
      final GM_Triangle_Impl triangle = createTriangle( nodes, m_parameter );
      if( triangle != null )
        m_marshaller.marshalTriangle( triangle, m_crs );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater.1" ), e ); //$NON-NLS-1$
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    }
  }

  public static GM_Triangle_Impl createTriangle( final INodeResult[] nodes, final ResultType.TYPE parameter ) throws GM_Exception
  {
    if( nodes.length < 3 )
      return null;

    final GM_Position pos[] = processNodes( nodes, parameter );
    if( pos != null )
    {
      final String crs = nodes[0].getPoint().getCoordinateSystem();
      return new GM_Triangle_Impl( pos[0], pos[1], pos[2], crs );
    }

    return null;
  }

  private static GM_Position[] processNodes(final INodeResult[] nodes, ResultType.TYPE parameter )
  {
    // if no parameter is set, use terrain
    if( parameter == null )
      parameter = ResultType.TYPE.TERRAIN;

    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < nodes.length; i++ )
    {
      final INodeResult nodeResult = nodes[i];
      final GM_Point point = nodeResult.getPoint();
      final double x = point.getX();
      final double y = point.getY();
      final double z = getZValue( nodeResult, parameter );

      pos[i] = GeometryFactory.createGM_Position( x, y, z );
    }
    return pos;
  }

  private static double getZValue( final INodeResult nodeResult, final ResultType.TYPE parameter )
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

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#finished()
   */
  public void finished( ) throws CoreException
  {
    final OutputStream os = m_xmlStream.getOutputStream();
    try
    {
      m_marshaller.endSurface();

      final AttributesImpl atts = new AttributesImpl();
      for( final QNameAndString prop : m_props )
      {
        final QName qname = prop.m_qname;
        m_xmlStream.startElement( qname.getNamespaceURI(), qname.getLocalPart(), qname.getLocalPart(), atts );
        m_xmlStream.characters( prop.m_value.toCharArray(), 0, prop.m_value.length() );
        m_xmlStream.endElement( qname.getNamespaceURI(), qname.getLocalPart(), qname.getLocalPart() );
      }

      m_xmlStream.endElement( "", "triangulatedSurfaceMember", "triangulatedSurfaceMember" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      m_xmlStream.endElement( "", "TinResult", "TinResult" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      m_xmlStream.endDocument();

      if( os instanceof ZipOutputStream )
        ((ZipOutputStream) os).closeEntry();
      os.close();
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater.0" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#setTime(java.util.Date)
   */
  public void setTime( final Date date )
  {
    final Calendar calendar = Calendar.getInstance();
    calendar.setTime( date );
    final String printedDateTime = DatatypeConverter.printDateTime( calendar );
    m_props.add( new QNameAndString( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "date" ), printedDateTime ) );//$NON-NLS-1$
  }

  /**
   * Returns the marshaller used to write the triangle, initialises it (and hence opens the file), if not yet done
   */
  private TriangulatedSurfaceMarshaller initMarshaller( ) throws CoreException
  {
    try
    {
      final String tinFilePath = m_tinResultFile.getPath();
      final String tinFileBase = FileUtilities.nameWithoutExtension( tinFilePath ) + "_" + m_parameter.name(); //$NON-NLS-1$

      final String extension = FilenameUtils.getExtension( tinFilePath ).toLowerCase();

      // Create zipped or normal file
      final OutputStream os;
      if( "zip".equals( extension ) ) //$NON-NLS-1$
      {
        os = new ZipOutputStream( new FileOutputStream( tinFileBase + ".zip" ) ); //$NON-NLS-1$
        m_zipEntry = new ZipEntry( "tin_" + m_parameter.name() + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
        ((ZipOutputStream) os).putNextEntry( m_zipEntry );
      }
      else if( "gz".equals( extension ) ) //$NON-NLS-1$
        os = new GZIPOutputStream( new BufferedOutputStream( new FileOutputStream( new File( tinFileBase + ".gz" ) ) ) );
      else
        os = new BufferedOutputStream( new FileOutputStream( new File( tinFileBase + ".gml" ) ) ); //$NON-NLS-1$

      m_xmlStream = new ToXMLStream();
      m_xmlStream.setOutputStream( os );
      // Configure content handler. IMPORTANT: call after setOutputStream!
      m_xmlStream.setLineSepUse( true );
      m_xmlStream.setIndent( true );
      m_xmlStream.setIndentAmount( 1 );

      final XMLReader xmlReader = XMLReaderFactory.createXMLReader();
      xmlReader.setContentHandler( m_xmlStream );

      m_marshaller = new TriangulatedSurfaceMarshaller( xmlReader, null );

      m_xmlStream.startDocument();

      m_xmlStream.startPrefixMapping( "gml", NS.GML3 ); // the attribute does not trigger the prefix mapping //$NON-NLS-1$
      m_xmlStream.startElement( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult", "TinResult" ); //$NON-NLS-1$ //$NON-NLS-2$
      m_xmlStream.addAttribute( NS.GML3, "id", "gml:id", "string", "root" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      m_xmlStream.startElement( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember", "triangulatedSurfaceMember" ); //$NON-NLS-1$ //$NON-NLS-2$

      final AttributesImpl atts = new AttributesImpl();
      if( m_crs != null )
      {
        atts.addAttribute( "", "srsName", "srsName", "CDATA", m_crs ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        atts.addAttribute( "", "srsDimension", "srsDimension", "CDATA", "" + CRSHelper.getDimension( m_crs ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      }

      m_marshaller.startSurface( atts );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater.1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    return m_marshaller;
  }

}
