package org.kalypso.ogc.sensor.view.resourceNavigator;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.util.xml.XMLTools;

/**
 * @author schlienger
 */
public class GrafikViewActionDelegate implements IViewActionDelegate
{
  private IFile m_currentFile = null;

  public GrafikViewActionDelegate()
  {
    // empty
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( IViewPart view )
  {
    // empty
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    if( m_currentFile != null )
    {
      // TODO: bessere konfigurierbarkeit von der Transformation?
      InputStream xsl = GrafikViewActionDelegate.class.getResourceAsStream( "/org/kalypso/plugin/resources/xsl/grafik-vorlage.xsl" );
      
      try
      {
        File grafikExe = FileUtilities.makeFileFromStream( false, "grafik", ".exe", GrafikViewActionDelegate.class.getResourceAsStream( "/org/kalypso/plugin/resources/exe/grafik.exe"), true );
        
        String str = XMLTools.xslTransform( m_currentFile.getContents(), xsl );
        
        File file = File.createTempFile( "grafik", "vorlage", grafikExe.getParentFile() );
        file.deleteOnExit();
        
        FileWriter fw = new FileWriter( file );
        fw.write( str );
        fw.close();
        
        Runtime.getRuntime().exec( grafikExe.getAbsolutePath() + " /V" + file.getAbsolutePath() );
      }
      catch( Exception e )
      {
        throw new RuntimeException( e );
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    IStructuredSelection sel = (IStructuredSelection)selection;
    
    if( sel.getFirstElement() instanceof IFile )
      m_currentFile = (IFile)sel.getFirstElement();
    else
      m_currentFile = null;
  }
}
