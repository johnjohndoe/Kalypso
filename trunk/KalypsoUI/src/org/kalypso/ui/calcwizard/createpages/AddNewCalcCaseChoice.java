package org.kalypso.ui.calcwizard.createpages;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.ModelNature;

/**
 * Die Implementierung erzeugt einen völlig neuen Rechenfall im
 * Prognoseverzeichnis
 * 
 * @author belger
 */
public class AddNewCalcCaseChoice implements IAddCalcCaseChoice
{
  private Control m_control;

  private final String m_label;

  private final IProject m_project;

  private final AddCalcCasePage m_page;

  public static final String TOOLTIP = "Geben Sie hier die Bezeichnung des Rechenfalls ein";

  private static final SimpleDateFormat m_format = new SimpleDateFormat( "yyyy-MM-dd_HH'h'mm" );

  protected String m_name;

  private Text m_edit;

  public AddNewCalcCaseChoice( final String label, final IProject project,
      final AddCalcCasePage page )
  {
    m_label = label;
    m_project = project;
    m_page = page;
    m_page.getClass();
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "Bezeichnung: " );
    label.setToolTipText( TOOLTIP );

    final GridData labelData = new GridData();
    labelData.grabExcessHorizontalSpace = false;
    labelData.horizontalAlignment = GridData.BEGINNING;
    label.setLayoutData( labelData );

    final Text edit = new Text( panel, SWT.BORDER );
    edit.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    edit.setToolTipText( TOOLTIP );
    edit.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        setName( edit.getText() );
      }
    } );
    m_edit = edit;

    m_control = panel;

    try
    {
      refresh( new NullProgressMonitor() );
    }
    catch( final CoreException e1 )
    {
      e1.printStackTrace();
    }
  }

  protected void setName( final String text )
  {
    m_name = text;
    
    validateChoice();
  }
  
  public void validateChoice()
  {
    final IStatus status = m_project.getWorkspace().validateName( m_name, IResource.FOLDER );
    if( status.getSeverity() == IStatus.OK )
    {
      m_page.setErrorMessage( null );
      m_page.setMessage( null );
      m_page.setPageComplete( true );
    }
    else
    {
      m_page.setErrorMessage( status.getMessage() );
      m_page.setMessage( null );
      m_page.setPageComplete( false );
    }
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#perform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );

    final IFolder prognoseFolder = nature.getPrognoseFolder();
    if( m_name.length() == 0 )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Geben Sie einen Namen für die Vorhersage ein", null ) );
    final IFolder calcCaseFolder = prognoseFolder.getFolder( m_name );
    if( calcCaseFolder.exists() )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Eine Vorhersage mit diesem Namen existiert bereits: " + m_name, null ) );

    nature.createCalculationCaseInFolder( calcCaseFolder, monitor );

    return calcCaseFolder;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#getControl()
   */
  public Control getControl()
  {
    return m_control;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#toString()
   */
  public String toString()
  {
    return m_label;
  }

  /**
   * @throws CoreException
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#refresh(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void refresh( final IProgressMonitor monitor ) throws CoreException
  {
    final StringBuffer buffer = new StringBuffer( "Vorhersage-" );

    buffer.append( m_format.format( new Date() ) );

    final String newName = createNewName( buffer.toString() );

    final Text edit = m_edit;
    if( !edit.isDisposed() )
    {
      edit.getDisplay().syncExec( new Runnable()
      {
        public void run()
        {
          edit.setText( newName );
        }
      } );
    }
  }

  private String createNewName( final String dateString ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );
    final IFolder prognoseFolder = nature.getPrognoseFolder();
    final IResource[] resources = prognoseFolder.exists() ? prognoseFolder.members() : new IResource[] {};
    
    int count = 0;
    while( true )
    {
      final String newName = count == 0 ? dateString : ( dateString  + "_#" + count );
      
      boolean bFound = false;
      for( int i = 0; i < resources.length; i++ )
      {
        if( resources[i].getName().equals( newName ) )
        {
          bFound = true;
          break;
        }
      }

      if( !bFound )
        return newName;

      count++;
    }
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#shouldUpdate()
   */
  public boolean shouldUpdate()
  {
    return true;
  }
}