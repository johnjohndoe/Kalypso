package org.kalypso.ui.wizard.calccase;

import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.kalypso.eclipse.core.resources.IProjectProvider;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.nature.ModelNature;

/**
 * Wizard-Page zur Eingabe der Steuerparameter
 * 
 * @author belger
 */
public class SteuerparameterWizardPage extends WizardPage
{
  private final IProjectProvider m_projectProvider;

  private GMLWorkspace m_controlGML = null;

  public SteuerparameterWizardPage( final String pageName, final IProjectProvider pp )
  {
    super( pageName, "Steurparameter", null );

    m_projectProvider = pp;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      final IProject project = m_projectProvider.getProject();
      final ModelNature nature = (ModelNature)project.getNature( ModelNature.ID );
      m_controlGML = nature.getDefaultControl();

      // Vorlage auslesen
      final URL viewURL = new URL( "platform:/resource/" + project.getName() + "/"
          + ModelNature.CONTROL_VIEW_FILE );
      final Control control = new FeatureComposite( m_controlGML.getRootFeature(), new URL[]
      { viewURL } ).createControl( parent, SWT.NONE );
      setControl( control );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      // TODO: error handling?
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
  }

  public boolean saveControl( final IFolder folder )
  {
    final IFile file = folder.getFile( ModelNature.CALCULATION_FILE );
    final Feature rootFeature = m_controlGML.getRootFeature();

    WorkspaceModifyOperation op = new WorkspaceModifyOperation( null )
    {
      public void execute( final IProgressMonitor monitor ) throws CoreException
      {
        try
        {
          monitor.beginTask( "Steuerparameter speichern", 3000 ); //$NON-NLS-1$

          new SetContentThread( file, false, true, monitor )
          {
            public void writeStream() throws Throwable
            {
              final Writer controlWriter = new OutputStreamWriter( getOutputStream() );
              GmlSerializer.serializeFeature( controlWriter, rootFeature, monitor );
            }
          };
        }
        finally
        {
          monitor.done();
        }
      }
    };

    try
    {
      getContainer().run( true, true, op );
    }
    catch( final InterruptedException e )
    {
      if( file.exists() )
        try
        {
          file.delete( true, false, null );
        }
        catch( CoreException e1 )
        {
          e1.printStackTrace();
        }

      return false;
    }
    catch( final InvocationTargetException e )
    {
      if( e.getTargetException() instanceof CoreException )
      {
        ErrorDialog.openError( getContainer().getShell(), // Was
            // Utilities.getFocusShell()
            IDEWorkbenchMessages.getString( "WizardNewFolderCreationPage.errorTitle" ), //$NON-NLS-1$
            null, // no special message
            ( (CoreException)e.getTargetException() ).getStatus() );
      }
      else
      {
        // CoreExceptions are handled above, but unexpected runtime exceptions
        // and errors may still occur.

        IDEWorkbenchPlugin
            .log( MessageFormat
                .format(
                    "Exception in {0}.getNewFolder(): {1}", new Object[] { getClass().getName(), e.getTargetException() } ) );//$NON-NLS-1$
        MessageDialog
            .openError(
                getContainer().getShell(),
                IDEWorkbenchMessages.getString( "WizardNewFolderCreationPage.internalErrorTitle" ), IDEWorkbenchMessages.format( "WizardNewFolder.internalError", new Object[] { e.getTargetException().getMessage() } ) ); //$NON-NLS-2$ //$NON-NLS-1$
      }

      if( file.exists() )
        try
        {
          file.delete( true, false, null );
        }
        catch( final CoreException e1 )
        {
          e1.printStackTrace();
        }

      return false;
    }

    return true;
  }
}