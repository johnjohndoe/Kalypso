package org.kalypso.ogc.sensor.view.resourceNavigator;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.java.io.FileUtilities;

/**
 * Startet das Grafik-Tool direkt auf die Grafikvorlage (*.tpl-Datei).
 * 
 * @author schlienger
 */
public class GrafikVorlageActionDelegate implements IViewActionDelegate
{
  private IFile m_currentFile = null;

  public GrafikVorlageActionDelegate()
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
      try
      {
        File grafikExe = FileUtilities.makeFileFromStream( false, "grafik", ".exe", GrafikVorlageActionDelegate.class.getResourceAsStream( "/org/kalypso/plugin/resources/exe/grafik.exe_"), true );
        
        Runtime.getRuntime().exec( grafikExe.getAbsolutePath() + " /V" + m_currentFile.getLocation().toOSString() );
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
