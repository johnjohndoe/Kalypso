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

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.ZipOutputStream;

import javax.xml.namespace.QName;

import org.apache.xml.serializer.ToXMLStream;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.transformation.CRSHelper;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.io.sax.marshaller.TriangulatedSurfaceMarshaller;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * @author Gernot Belger
 */
public class TinResultWriter
{
  /**
   * Just a pair of qname and a string.<br>
   * Used as additional property values for the root feature of the surface.
   */
  public final static class QNameAndString
  {
    public final QName m_qname;

    public final String m_value;

    public QNameAndString( final QName qname, final String value )
    {
      m_qname = qname;
      m_value = value;
    }
  }

  private TriangulatedSurfaceMarshaller m_marshaller;

  private ToXMLStream m_xmlStream;

  private final String m_crs;

  private final List<QNameAndString> m_props = new ArrayList<>();

  public TinResultWriter( final OutputStream os, final String crs, final QNameAndString[] props ) throws CoreException
  {
    m_crs = crs;
    if( props != null )
      m_props.addAll( Arrays.asList( props ) );
    initMarshaller( os );
  }

  /**
   * add a triangle to the eater. The triangle is defined by its three nodes ({@link INodeResult} and a information, if
   * the triangle is marked as wet or dry.
   * 
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#add(java.util.List)
   */
  public void add( final GM_Position... nodes ) throws SAXException
  {
    if( m_marshaller == null )
      return;

    if( nodes.length != 3 )
      return;

    final GM_Triangle triangle = GeometryFactory.createGM_Triangle( nodes[0], nodes[1], nodes[2], m_crs );
    if( triangle != null )
    {
      m_marshaller.marshallTriangle( triangle );
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

      m_xmlStream.endElement( "", "triangulatedSurfaceMember", "triangulatedSurfaceMember" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      final AttributesImpl atts = new AttributesImpl();
      for( final QNameAndString prop : m_props )
      {
        final QName qname = prop.m_qname;
        m_xmlStream.startElement( qname.getNamespaceURI(), qname.getLocalPart(), qname.getLocalPart(), atts );
        m_xmlStream.characters( prop.m_value.toCharArray(), 0, prop.m_value.length() );
        m_xmlStream.endElement( qname.getNamespaceURI(), qname.getLocalPart(), qname.getLocalPart() );
      }

      m_xmlStream.endElement( "", "TinResult", "TinResult" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      m_xmlStream.endDocument();

      if( os instanceof ZipOutputStream )
        ((ZipOutputStream)os).closeEntry();
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater.0" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  /**
   * Returns the marshaller used to write the triangle, initialises it (and hence opens the file), if not yet done
   */
  private void initMarshaller( final OutputStream os ) throws CoreException
  {
    try
    {
      m_xmlStream = new ToXMLStream();
      m_xmlStream.setOutputStream( os );
      // Configure content handler. IMPORTANT: call after setOutputStream!
      m_xmlStream.setLineSepUse( true );
      m_xmlStream.setIndent( true );
      m_xmlStream.setIndentAmount( 1 );

      final XMLReader xmlReader = XMLReaderFactory.createXMLReader();
      xmlReader.setContentHandler( m_xmlStream );

      m_marshaller = new TriangulatedSurfaceMarshaller( xmlReader, m_crs );

      m_xmlStream.startDocument();

      m_xmlStream.setEncoding( "UTF-8" ); //$NON-NLS-1$
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

      m_marshaller.startSurface( null, atts );
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceDirectTriangleEater.1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  public void addProperty( final QNameAndString nameAndString )
  {
    m_props.add( nameAndString );
  }
}
