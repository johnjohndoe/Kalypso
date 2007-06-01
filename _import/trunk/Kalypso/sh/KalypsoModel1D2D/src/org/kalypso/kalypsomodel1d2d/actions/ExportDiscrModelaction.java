package org.kalypso.kalypsomodel1d2d.actions;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;

public class ExportDiscrModelaction implements IObjectActionDelegate
{
  private IFeatureSelection m_selection;

  private IWorkbenchPart m_targetPart;  
  
  public void run( IAction action )
  {
    final Feature[] features = FeatureSelectionHelper.getFeatures( m_selection );
    final Feature discFeature = features[0];

    final IFEDiscretisationModel1d2d discModel  = (IFEDiscretisationModel1d2d) discFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    
    /* open file dialog */
    final Shell shell = m_targetPart.getSite().getShell();
    
    final FileDialog dialog = new FileDialog(shell, SWT.SAVE );
    
    dialog.setText( "Model Export" );
    dialog.setFilterExtensions( new String[] { "*.2d", "*.*" } );
    dialog.setFilterNames( new String[] { "BCE2D-Format (*.2d)", "*.*" }  );
    dialog.open();

    final String filterPath = dialog.getFilterPath();
    final String fileName = dialog.getFileName();
    final String name = fileName; 
   
    File modelFile = new File( filterPath, name );
    
    if( modelFile.exists() )
    {
      if( !MessageDialog.openQuestion( shell, "Modelexport", "Datei existiert bereits. Überschreiben?" ) )
        return;
    }

    final Gml2RMA10SConv converter2D = new Gml2RMA10SConv( modelFile, discModel, null, null,null );
    
    try
    {
      converter2D.toRMA10sModel();
    }
    catch( IllegalStateException e )
    {
      e.printStackTrace();
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
    }

  }

  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    final boolean enable = m_selection != null && m_selection.size() == 1;
    action.setEnabled( enable );
  }

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction, org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_targetPart = targetPart;
    
    action.setEnabled( m_targetPart != null );
  }

}
