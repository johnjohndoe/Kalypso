package org.kalypso.ui.calcwizard;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.eclipse.core.resources.IProjectProvider;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.model.xml.Calcwizard;
import org.kalypso.model.xml.CalcwizardType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.calcwizard.createpages.AddCalcCasePage;
import org.kalypso.ui.calcwizard.createpages.AddNewCalcCaseChoice;
import org.kalypso.ui.calcwizard.createpages.ContinueOldCalcCaseChoice;
import org.kalypso.ui.calcwizard.createpages.CopyCalcCaseChoice;
import org.kalypso.ui.calcwizard.modelpages.IModelWizardPage;
import org.kalypso.ui.nature.ModelNature;
import org.kalypso.ui.wizard.calccase.SteuerparameterWizardPage;
import org.xml.sax.InputSource;

public class CalcWizard implements IWizard, IProjectProvider
{
  private final IProject m_project;

  protected AddCalcCasePage m_addCalcCasePage;

  protected SteuerparameterWizardPage m_controlPage;

  private List m_pages = new ArrayList();

  private IWizardContainer m_container;

  private IDialogSettings m_dialogSettings;

  public CalcWizard( final IProject project )
  {
    m_project = project;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    m_addCalcCasePage = new AddCalcCasePage();
    m_controlPage = new SteuerparameterWizardPage( this, true );
    
    m_addCalcCasePage.addChoice( new AddNewCalcCaseChoice( "einen neuen Rechenfall erzeugen", m_project, m_addCalcCasePage ) );
    m_addCalcCasePage.addChoice( new ContinueOldCalcCaseChoice( "einen bereits vorhandenen Rechenfall fortführen", m_project, m_addCalcCasePage ) );
    m_addCalcCasePage.addChoice( new CopyCalcCaseChoice( "einen bereits vorhandenen Rechenfall kopieren", m_project, m_addCalcCasePage ) );

    addPage( m_addCalcCasePage );
    addPage( m_controlPage );
  }

  public void addPage( final IWizardPage page )
  {
    m_pages.add( page );
    page.setWizard( this );
  }

  protected void addModelPages( final IFolder calcCaseFolder, final IProgressMonitor monitor )
      throws CoreException
  {
    try
    {
      monitor.beginTask( "Seiten zur Modellbearbeitung werden geladen", 2000 );

      // wizard seiten hinzufügen!
      final IFile wizardConfigFile = (IFile)m_project
          .findMember( ModelNature.MODELLTYP_CALCWIZARD_XML );
      final InputSource inputSource = new InputSource( wizardConfigFile.getContents() );
      inputSource.setEncoding( wizardConfigFile.getCharset() );

      final Calcwizard calcwizardData = (Calcwizard)new ObjectFactory().createUnmarshaller()
          .unmarshal( inputSource );

      monitor.worked( 1000 );

      final java.util.List pages = calcwizardData.getPage();
      for( final Iterator pIt = pages.iterator(); pIt.hasNext(); )
      {
        final CalcwizardType.PageType page = (CalcwizardType.PageType)pIt.next();

        final Properties props = new Properties();
        final List arglist = page.getArg();
        for( Iterator aIt = arglist.iterator(); aIt.hasNext(); )
        {
          final CalcwizardType.PageType.ArgType arg = (CalcwizardType.PageType.ArgType)aIt.next();
          props.setProperty( arg.getName(), arg.getValue() );
        }

        final String className = page.getClassName();
        final String pageTitle = page.getPageTitle();
        final String imageLocation = page.getImageLocation();
        final ImageDescriptor imageDesc = imageLocation == null ? null : ImageProvider
            .id( imageLocation );

        final IModelWizardPage wizardPage = (IModelWizardPage)ClassUtilities.newInstance(
            className, IModelWizardPage.class, ModelNature.class.getClassLoader(), null, null );
        wizardPage.init( m_project, pageTitle, imageDesc, props, calcCaseFolder );

        addPage( wizardPage );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Erzeugen der Seiten zur Modellbearbeitung", e ) );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    //    final IWizardPage[] pages = getPages();
    //    for( int i = 0; i < pages.length; i++ )
    //    {
    //      final IModelWizardPage page = (IModelWizardPage)pages[i];
    //      final boolean bFinished = page.performFinish();
    //      if( bFinished == false )
    //        return false;
    //    }

    // TODO eventuell noch anderes Zeugs machen

    return true; //deactivatePage( getContainer().getCurrentPage() );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performCancel()
   */
  public boolean performCancel()
  {
    // den Rechenfall löschen
    //    final IFolder calcFolder = m_folder;
    //    final Job job = new Job( "Rechenfall löschen" )
    //    {
    //      protected IStatus run( IProgressMonitor monitor )
    //      {
    //        try
    //        {
    //          calcFolder.delete( false, false, monitor );
    //        }
    //        catch( final CoreException e )
    //        {
    //          return e.getStatus();
    //        }
    //        
    //        return Status.OK_STATUS;
    //      }
    //    };
    //    job.schedule();
    //    try
    //    {
    //      job.join();
    //    }
    //    catch( final InterruptedException e )
    //    {
    //      e.printStackTrace();
    //    }

    // alle cleanen?

    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#canFinish()
   */
  public boolean canFinish()
  {
    // TODO only finish on last page!
    return false;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#dispose()
   */
  public void dispose()
  {
    // notify pages
    for( int i = 0; i < m_pages.size(); i++ )
    {
      ( (IWizardPage)m_pages.get( i ) ).dispose();
    }
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getPageCount()
   */
  public int getPageCount()
  {
    return m_pages.size();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getStartingPage()
   */
  public IWizardPage getStartingPage()
  {
    if( m_pages.size() == 0 )
      return null;
    return (IWizardPage)m_pages.get( 0 );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getNextPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public boolean doNext( final IWizardPage page )
  {
    final WorkspaceModifyOperation op = new WorkspaceModifyOperation( null )
    {
      public void execute( final IProgressMonitor monitor ) throws CoreException
      {
        if( page == m_addCalcCasePage )
        {
          // vielleicht sollte das hier auch erst nach den Steuerparametern passieren?
          m_addCalcCasePage.doNext( monitor );
          m_controlPage.setUpdate( m_addCalcCasePage.shouldUpdate() );
        }
        else if( page == m_controlPage )
        {
          monitor.beginTask( "aktualisiere Zeitreihen", 3000 );

          final IFolder currentCalcCase = m_addCalcCasePage.getCurrentCalcCase();
          m_controlPage.saveChanges( currentCalcCase, new SubProgressMonitor( monitor, 1000 ) );
          if( m_controlPage.isUpdate() )
            ModelNature.updateCalcCase( currentCalcCase, new SubProgressMonitor( monitor, 1000 ) );
          else
            monitor.worked( 1000 );
          
          addModelPages( currentCalcCase, new SubProgressMonitor( monitor, 1000 ) );
        }
      }
    };

    try
    {
      getContainer().run( true, true, op );
    }
    catch( final InterruptedException e )
    {
      // canceled
      // TODO error message?
      return false;
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final Throwable te = e.getTargetException();
      if( te instanceof CoreException )
      {
        ErrorDialog.openError( getContainer().getShell(), "Fehler", "Fehler beim Aufruf der nächsten Wizard-Seite",
            ( (CoreException)e.getTargetException() ).getStatus() );
      }
      else
      {
        // CoreExceptions are handled above, but unexpected runtime exceptions
        // and errors may still occur.
        MessageDialog.openError( getContainer().getShell(), "Interner Fehler",
            "Fehler beim Aufruf der nächsten Wizard-Seite: " + te.getLocalizedMessage() );
      }
      
      return false;
    }
    return true;
  }
    
  /**
   * @see org.eclipse.jface.wizard.IWizard#getNextPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public IWizardPage getNextPage( final IWizardPage page )
  {
    final int index = m_pages.indexOf( page );
    if( index == m_pages.size() - 1 || index == -1 )
      // last page or page not found
      return null;

    return (IWizardPage)m_pages.get( index + 1 );
  }

  public boolean doBack( final IWizardPage page )
  {
    final IWizardPage previous = getPreviousPage( page );
    if( previous instanceof ICalcWizardPage )
    {
      try
      {
        ((ICalcWizardPage)previous).update( new NullProgressMonitor() );
        return true;
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
        
        return false;
      }
    }
    
    return true;
  }
  
  /**
   * @see org.eclipse.jface.wizard.IWizard#getPreviousPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public IWizardPage getPreviousPage( final IWizardPage page )
  {
    final int index = m_pages.indexOf( page );
    if( index == 0 || index == -1 )
      // first page or page not found
      return null;

    return (IWizardPage)m_pages.get( index - 1 );
  }

  /**
   * @see org.kalypso.eclipse.core.resources.IProjectProvider#getProject()
   */
  public IProject getProject()
  {
    return m_project;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  public void createPageControls( final Composite pageContainer )
  {
  // nichts tun, no demand!
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getContainer()
   */
  public IWizardContainer getContainer()
  {
    return m_container;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getDefaultPageImage()
   */
  public Image getDefaultPageImage()
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getDialogSettings()
   */
  public IDialogSettings getDialogSettings()
  {
    return m_dialogSettings;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getPage(java.lang.String)
   */
  public IWizardPage getPage( final String pageName )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getPages()
   */
  public IWizardPage[] getPages()
  {
    return (IWizardPage[])m_pages.toArray( new IWizardPage[m_pages.size()] );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getTitleBarColor()
   */
  public RGB getTitleBarColor()
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getWindowTitle()
   */
  public String getWindowTitle()
  {
    return "Hochwasser Vorhersage für " + m_project.getName();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#isHelpAvailable()
   */
  public boolean isHelpAvailable()
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#needsPreviousAndNextButtons()
   */
  public boolean needsPreviousAndNextButtons()
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#needsProgressMonitor()
   */
  public boolean needsProgressMonitor()
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#setContainer(org.eclipse.jface.wizard.IWizardContainer)
   */
  public void setContainer( final IWizardContainer wizardContainer )
  {
    m_container = wizardContainer;
  }

}