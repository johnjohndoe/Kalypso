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
package org.kalypso.ogc.gml.outline;

import java.io.File;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ogc.gml.GisTemplateUserStyle;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.w3c.dom.Document;

/**
 * @author belger
 */
public class SaveStyleAction extends AbstractOutlineAction
{
  private final Shell shell;

  public SaveStyleAction( final String text, final ImageDescriptor image, final String tooltipText, final GisMapOutlineViewer outlineViewer )
  {
    super( text, image, tooltipText, outlineViewer, null );
    shell = outlineViewer.getControl().getShell();
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */

  @Override
  public void run( )
  {
    Object o = ((IStructuredSelection) getOutlineviewer().getSelection()).getFirstElement();
    if( o instanceof ThemeStyleTreeObject )
    {
      final IKalypsoTheme theme = ((ThemeStyleTreeObject) o).getTheme();
      if( theme instanceof IKalypsoFeatureTheme )
      {
        KalypsoUserStyle kalypsoStyle = ((ThemeStyleTreeObject) o).getStyle();
        saveUserStyle( kalypsoStyle, shell );
      }
    }
  }

  public static void saveUserStyle( KalypsoUserStyle userStyle, Shell shell )
  {
    File knownFilename = null;
    URL url = null;
    if( userStyle instanceof GisTemplateUserStyle )
    {
      try
      {
        GisTemplateUserStyle tus = (GisTemplateUserStyle) userStyle;
        final PoolableObjectType poolKey = tus.getPoolKey();
        final URL context = poolKey == null ? null : poolKey.getContext();
        final String location = poolKey == null ? null : poolKey.getLocation();
        if( location != null )
        {
          final UrlResolver resolver = new UrlResolver();
          url = resolver.resolveURL( context, location );
          final IFile file = ResourceUtilities.findFileFromURL( url );
          knownFilename = ResourceUtilities.makeFileFromPath( file.getFullPath() );
        }
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
      }
    }
    final URL context = url;
    final String[] filterExtension = { "*.sld" };
    FileDialog saveDialog = new FileDialog( shell, SWT.SAVE );
    saveDialog.setFilterExtensions( filterExtension );

    if( knownFilename != null )
      saveDialog.setFileName( knownFilename.getAbsolutePath() );
    String sldContents = "<StyledLayerDescriptor version=\"String\" xmlns=\"http://www.opengis.net/sld\" xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><NamedLayer><Name>deegree style definition</Name>";
    sldContents += userStyle.exportAsXML();
    sldContents += "</NamedLayer></StyledLayerDescriptor>";
    StyledLayerDescriptor sld;
    try
    {
      final IUrlResolver2 resolver = new IUrlResolver2()
      {
        public URL resolveURL( String href ) throws MalformedURLException
        {
          return UrlResolverSingleton.resolveUrl( context, href );
        }

      };
      sld = SLDFactory.createSLD( resolver, sldContents );
      final String filename = saveDialog.open();
      if( filename != null )
      {
        Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
        final Source source = new DOMSource( doc );
        File file = null;
        if( filename.indexOf( "." ) == -1 )
          file = new File( filename + ".sld" );
        else
          file = new File( filename );
        IFile iFile = ResourceUtilities.findFileFromURL( file.toURL() );

        if( iFile != null )
        {
          // TODO dialog, der einen IFile zurueckliefert, damit ein refresh durchgefuert wird
          final SetContentHelper thread = new SetContentHelper()
          {
            @Override
            protected void write( final OutputStreamWriter writer ) throws Throwable
            {

              final Transformer t = TransformerFactory.newInstance().newTransformer();
              t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
              t.setOutputProperty( OutputKeys.INDENT, "yes" );
              t.transform( source, new StreamResult( writer ) );
            }
          };
          thread.setFileContents( iFile, false, true, new NullProgressMonitor() );
        }
        else if( file != null )
        {
          // outside workspace
          Result result = new StreamResult( file );
          Transformer t = TransformerFactory.newInstance().newTransformer();
          t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
          t.setOutputProperty( OutputKeys.INDENT, "yes" );
          t.transform( source, result );
        }
      }
    }
    catch( final CoreException ce )
    {
      ce.printStackTrace();
      ErrorDialog.openError( shell, "Fehler", "Fehler beim Speichern des Styles", ce.getStatus() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  @Override
  protected final void refresh( )
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection) getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof ThemeStyleTreeObject )
      bEnable = true;

    setEnabled( bEnable );
  }
}