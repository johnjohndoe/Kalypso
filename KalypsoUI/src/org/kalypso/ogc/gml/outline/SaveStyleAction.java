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
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.GisTemplateUserStyle;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.UserStyleTreeObject;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
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
  public SaveStyleAction( final String text, final ImageDescriptor image, final String tooltipText, final IMapModellView outlineViewer )
  {
    super( text, image, tooltipText, outlineViewer );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */

  @Override
  public void run( )
  {
    final Object o = ((IStructuredSelection) getOutlineviewer().getSelection()).getFirstElement();
    if( o instanceof UserStyleTreeObject )
    {
      final IKalypsoTheme theme = ((UserStyleTreeObject) o).getParent();
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final KalypsoUserStyle kalypsoStyle = ((UserStyleTreeObject) o).getStyle();
        saveUserStyle( kalypsoStyle, PlatformUI.getWorkbench().getDisplay().getActiveShell() );
      }
    }
  }

  public static void saveUserStyle( final KalypsoUserStyle userStyle, final Shell shell )
  {
    File knownFilename = null;
    URL url = null;
    if( userStyle instanceof GisTemplateUserStyle )
    {
      try
      {
        final GisTemplateUserStyle tus = (GisTemplateUserStyle) userStyle;
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
      catch( final Exception e1 )
      {
        e1.printStackTrace();
      }
    }
    final URL context = url;
    final String[] filterExtension = { "*.sld" }; //$NON-NLS-1$
    final FileDialog saveDialog = new FileDialog( shell, SWT.SAVE );
    saveDialog.setFilterExtensions( filterExtension );

    if( knownFilename != null )
      saveDialog.setFileName( knownFilename.getAbsolutePath() );
    String sldContents = "<StyledLayerDescriptor version=\"String\" xmlns=\"http://www.opengis.net/sld\" xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><NamedLayer><Name>deegree style definition</Name>"; //$NON-NLS-1$
    sldContents += userStyle.exportAsXML();
    sldContents += "</NamedLayer></StyledLayerDescriptor>"; //$NON-NLS-1$
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
        final Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
        final Source source = new DOMSource( doc );
        File file = null;
        if( filename.indexOf( "." ) == -1 ) //$NON-NLS-1$
          file = new File( filename + ".sld" ); //$NON-NLS-1$
        else
          file = new File( filename );
        final IFile iFile = ResourceUtilities.findFileFromURL( file.toURL() );

        if( iFile != null )
        {
          // TODO dialog, der einen IFile zurueckliefert, damit ein refresh durchgefuert wird
          final SetContentHelper thread = new SetContentHelper()
          {
            @Override
            protected void write( final OutputStreamWriter writer ) throws Throwable
            {

              final Transformer t = TransformerFactory.newInstance().newTransformer();
              t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
              t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
              t.transform( source, new StreamResult( writer ) );
            }
          };
          thread.setFileContents( iFile, false, true, new NullProgressMonitor() );
        }
        else if( file != null )
        {
          // outside workspace
          final Result result = new StreamResult( file );
          final Transformer t = TransformerFactory.newInstance().newTransformer();
          t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
          t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
          t.transform( source, result );
        }
      }
    }
    catch( final CoreException ce )
    {
      ce.printStackTrace();
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.ogc.gml.outline.SaveStyleAction.11" ), Messages.getString( "org.kalypso.ogc.gml.outline.SaveStyleAction.12" ), ce.getStatus() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  protected final void refresh( )
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection) getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof UserStyleTreeObject )
      bEnable = true;

    setEnabled( bEnable );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    // TODO Auto-generated method stub

  }
}