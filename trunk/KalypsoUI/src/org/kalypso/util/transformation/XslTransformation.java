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
package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.java.lang.CatchThread;
import org.w3c.dom.Document;

/**
 * @author belger
 */
public class XslTransformation extends AbstractTransformation
{
  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, java.io.BufferedWriter, java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( "XSL Transformation", 3000 );

    final String input = properties.getProperty( "input" );
    final String xsl = properties.getProperty( "xsl" );
    final String output = properties.getProperty( "output" );

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    final IFile inputFile = (IFile)root.findMember( input );
    final IFile xslFile = (IFile)root.findMember( xsl );
    final IFile outputFile = (IFile)root.findMember( output );

    try
    {
      final StreamSource xslSource = new StreamSource( xslFile.getContents(), xslFile.getCharset() );

      final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware( true );
      final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
      final Document xmlDOM = docuBuilder.parse( inputFile.getContents() );
      final TransformerFactory transformerFactory = TransformerFactory.newInstance();
      final Transformer transformer = transformerFactory.newTransformer( xslSource );

      final PipedInputStream pis = new PipedInputStream();
      final PipedOutputStream pos = new PipedOutputStream( pis );
      final CatchThread thread = new CatchThread()
      {
        protected void runIntern() throws Throwable
        {
          transformer.transform( new DOMSource( xmlDOM ), new StreamResult( pos ) );
          pos.close();
        }
      };
      thread.start();

      monitor.worked( 1000 );

      outputFile.create( pis, false, new SubProgressMonitor( monitor, 1000 ) );
      outputFile.setCharset( inputFile.getCharset(), new SubProgressMonitor( monitor, 1000 ) );

      if( thread.getThrown() != null )
        throw new TransformationException( thread.getThrown() );
    }
    catch( Exception e )
    {
      throw new TransformationException( e );
    }
  }

}