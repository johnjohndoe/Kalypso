package org.kalypso.ogc.gml.outline;

import java.io.File;
import java.io.StringReader;
import java.io.Writer;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.xml.XMLTools;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.deegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.KalypsoGisPlugin;
import org.w3c.dom.Document;

/**
 * @author belger
 */
public class SaveStyleAction extends AbstractOutlineAction
{
  private final Shell shell;

  public SaveStyleAction( final String text, final ImageDescriptor image, final String tooltipText,
      final GisMapOutlineViewer outlineViewer )
  {
    super( text, image, tooltipText, outlineViewer, null );
    shell = outlineViewer.getControl().getShell();
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */

  public void run()
  {
    Object o = ( (IStructuredSelection)getOutlineviewer().getSelection() ).getFirstElement();
    if( o instanceof ThemeStyleTreeObject )
    {
      final IKalypsoTheme theme = ( (ThemeStyleTreeObject)o ).getTheme();
      if( theme instanceof IKalypsoFeatureTheme )
      {
        KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
        saveUserStyle( kalypsoStyle, shell );
      }
    }
  }

  public static void saveUserStyle( KalypsoUserStyle userStyle, Shell shell )
  {
    String[] filterExtension =
    { "*.sld" };
    FileDialog saveDialog = new FileDialog( shell, SWT.SAVE );
    saveDialog.setFilterExtensions( filterExtension );
    String sldContents = "<StyledLayerDescriptor version=\"String\" xmlns=\"http://www.opengis.net/sld\" xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><NamedLayer><Name>deegree style definition</Name>";
    sldContents += userStyle.exportAsXML();
    sldContents += "</NamedLayer></StyledLayerDescriptor>";
    StyledLayerDescriptor sld;
    try
    {
      sld = SLDFactory.createSLD( sldContents );
      String filename = saveDialog.open();
      if( filename != null )
      {
        Document doc = XMLTools.parse( new StringReader( ( (StyledLayerDescriptor_Impl)sld )
            .exportAsXML() ) );
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
          final SetContentThread thread = new SetContentThread( iFile, !iFile.exists(), false,
              true, new NullProgressMonitor() )
          {
            protected void write( final Writer writer ) throws Throwable
            {

              final Transformer t = TransformerFactory.newInstance().newTransformer();
              t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
              t.setOutputProperty( OutputKeys.INDENT, "yes" );
              t.transform( source, new StreamResult( writer ) );
            }
          };
          thread.start();
          thread.join();

          final CoreException fileException = thread.getFileException();
          if( fileException != null )
            throw fileException;

          final Throwable thrown = thread.getThrown();
          if( thrown != null )
          {
            CoreException coreE = new CoreException( KalypsoGisPlugin.createErrorStatus(
                "Fehler beim Speichern", thrown ) );
            ErrorDialog.openError( shell, "Fehler", "Fehler beim Speichern des Styles", coreE
                .getStatus() );
          }
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
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  protected final void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof ThemeStyleTreeObject )
      bEnable = true;

    setEnabled( bEnable );
  }
}