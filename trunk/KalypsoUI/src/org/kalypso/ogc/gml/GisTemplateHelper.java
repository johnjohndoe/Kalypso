/*--------------- Kalypso-Header --------------------------------------------------------------------

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
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.types.ExtentType;
import org.xml.sax.InputSource;

/**
 * Hilfsklasse, um aus den Binding-Klassen 'echte' Objekte zu erzeugen und
 * umgekehrt
 * 
 * @author Belger
 */
public class GisTemplateHelper
{
  private GisTemplateHelper()
  {
  // never instantiate this class
  }

  public static final Featuretemplate loadGisFeatureTemplate( final IFile file,
      final Properties replaceProps ) throws CoreException, IOException, JAXBException
  {
    // TODO: replace with 'ReplaceToken'
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file
        .getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );

    return loadGisFeatureTemplate( new InputSource( new StringReader( contents ) ) );
  }

  public static final Featuretemplate loadGisFeatureTemplate( final InputSource is )
      throws JAXBException
  {
    Unmarshaller unmarshaller = new org.kalypso.template.featureview.ObjectFactory()
        .createUnmarshaller();
    return (Featuretemplate)unmarshaller.unmarshal( is );
  }

  public static final Gismapview loadGisMapView( final IFile file, final Properties replaceProps )
      throws CoreException, IOException, JAXBException
  {
    // TODO: replace with 'ReplaceToken'
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file
        .getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );

    return loadGisMapView( new InputSource( new StringReader( contents ) ) );
  }

  public static final Gismapview loadGisMapView( final IFile file ) throws JAXBException,
      CoreException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );

    return loadGisMapView( is );
  }

  public static final Gismapview loadGisMapView( final InputSource is ) throws JAXBException
  {
    final ObjectFactory objectFactory = new ObjectFactory();
    Unmarshaller unmarshaller = objectFactory.createUnmarshaller();

    return (Gismapview)unmarshaller.unmarshal( is );
  }

  /**
   * Führt ein Pattern-Ersetzen durch, bevor die Gistableview geparst wird Jeder
   * key der Properties wird durch seinen value ersetzt. Funktioniert nur
   * zeilenweise, d.h.
   * 
   * @throws CoreException
   * @throws CoreException
   * @throws IOException
   * @throws JAXBException
   */
  public static Gistableview loadGisTableview( final IFile file, final Properties replaceProps )
      throws CoreException, IOException, JAXBException
  {
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file
        .getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );

    return loadGisTableview( new InputSource( new StringReader( contents ) ) );
  }

  public static Gistableview loadGisTableview( final IFile file ) throws CoreException,
      JAXBException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );

    return loadGisTableview( is );
  }

  public static Gistableview loadGisTableview( final InputSource is ) throws JAXBException
  {
    Unmarshaller unmarshaller = new org.kalypso.template.gistableview.ObjectFactory()
        .createUnmarshaller();

    return (Gistableview)unmarshaller.unmarshal( is );
  }

  public static void saveGisMapView( final Gismapview modellTemplate, final OutputStream outStream )
      throws JAXBException
  {
    final ObjectFactory objectFactory = new ObjectFactory();
    final Marshaller marshaller = objectFactory.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    marshaller.marshal( modellTemplate, outStream );
  }

  public static GM_Envelope getBoundingBox( Gismapview gisview )
  {
    final ExtentType extent = gisview.getExtent();
    return GeometryFactory.createGM_Envelope( extent.getLeft(), extent.getBottom(), extent
        .getRight(), extent.getTop() );
  }
}