package org.kalypso.ui.nature;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.java.reflect.ClassUtilities;
import org.kalypso.java.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.model.transformation.ICalculationCaseTransformation;
import org.kalypso.model.transformation.TransformationException;
import org.kalypso.model.transformation.TransformationFactory;
import org.kalypso.model.xml.Calcwizard;
import org.kalypso.model.xml.CalcwizardType;
import org.kalypso.model.xml.Modelspec;
import org.kalypso.model.xml.ModelspecType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.TransformationConfig;
import org.kalypso.model.xml.TransformationType;
import org.kalypso.model.xml.CalcwizardType.PageType.ArgType;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.xml.sax.InputSource;

import com.sun.xml.bind.StringInputStream;

/**
 * 
 * @author belger
 */
public class ModelNature implements IProjectNature, IResourceChangeListener
{
  private static final String MODELLTYP_FOLDER = "modellTyp";

  private static final String MODELLTYP_CALCCASECONFIG_XML = MODELLTYP_FOLDER + "/"
      + "calcCaseConfig.xml";

  private static final String MODELLTYP_MODELSPEC_XML = MODELLTYP_FOLDER + "/" + "modelspec.xml";

  public static final String ID = "org.kalypso.ui.ModelNature";

  private static final String METADATA_FILE = ".metadata";

  public static final String CALCULATION_FILE = ".calculation";

  private static final String CALC_RESULT_FOLDER = ".results";

  private static final String MODELLTYP_CALCWIZARD_XML = MODELLTYP_FOLDER + "/" + "calcWizard.xml";

  private final Properties m_metadata = new Properties();

  private IProject m_project;

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure()
  {
  // nix tun
  }

  private IFile getMetadataFile()
  {
    return m_project.getFile( new Path( METADATA_FILE ) );
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure() throws CoreException
  {
    // TODO: wird nie aufgerufen!
    try
    {
      final IFile file = getMetadataFile();

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_metadata.store( bos, "Modell-Projekt Metadata Information" );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );

      if( file.exists() )
        file.setContents( bis, false, true, new NullProgressMonitor() );
      else
        file.create( bis, false, new NullProgressMonitor() );

      bis.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#getProject()
   */
  public IProject getProject()
  {
    return m_project;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
   */
  public void setProject( final IProject project )
  {
    if( m_project != null )
      ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    m_project = project;

    if( m_project != null )
    {
      ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
      try
      {
        reloadMetadata();
      }
      catch( final CoreException e )
      {
        // TODO: als job absetzen?
        e.printStackTrace();
      }
    }

  }

  public void dispose()
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
  }

  public static String checkCanCreateCalculationCase( final IPath path )
  {
    final IWorkspaceRoot resourceRoot = ResourcesPlugin.getWorkspace().getRoot();
    final IResource resource = resourceRoot.findMember( path );

    if( resource == null || resource == resourceRoot )
      return "Rechenfall muss innerhalb eines Projektordners angelegt werden";

    if( resource instanceof IFolder )
    {
      final IFolder folder = (IFolder)resource;
      if( isCalcCalseFolder( folder ) )
        return "Rechenfall darf nicht innerhalb eines anderen Rechenfalls angelegt werden";

      return checkCanCreateCalculationCase( folder.getParent().getFullPath() );
    }
    else if( resource instanceof IProject )
    {
      final IProject project = (IProject)resource;
      try
      {
        project.isNatureEnabled( ModelNature.ID );
        return null;
      }
      catch( CoreException e )
      {
        e.printStackTrace();

        return e.getMessage();
      }
    }

    return "???";
  }

  public static boolean isCalcCalseFolder( final IFolder folder )
  {
    final IResource calcFile = folder.findMember( CALCULATION_FILE );
    return ( calcFile != null && calcFile.exists() && calcFile instanceof IFile );
  }

  public static void createCalculationCaseInFolder( final IFolder folder,
      final IProgressMonitor monitor ) throws Exception
  {
    // maybe check requirements? (Project is of this nature an folder is not
    // contained in other CalcCase)

    tranformModelData( folder, monitor );

    final IFile calcFile = folder.getFile( CALCULATION_FILE );
    if( !calcFile.exists() )
      throw new Exception( "Es wurden keine Steuerparameter durch die Transformation erzeugt: "
          + calcFile.getName() );
  }

  private static void tranformModelData( final IFolder targetFolder, final IProgressMonitor monitor )
      throws JAXBException, CoreException, IOException, TransformationException
  {
    // load Transformer config
    final IFile tranformerConfigFile = (IFile)targetFolder.getProject().findMember(
        ModelNature.MODELLTYP_CALCCASECONFIG_XML );

    final InputStream contents = tranformerConfigFile.getContents();
    final TransformationConfig trans = (TransformationConfig)new ObjectFactory()
        .createUnmarshaller().unmarshal( contents );
    contents.close();

    final List transList = trans.getTransformation();

    monitor.beginTask( "Transformationen durchführen", transList.size() );

    for( Iterator iter = transList.iterator(); iter.hasNext(); )
    {
      final TransformationType element = (TransformationType)iter.next();
      final ICalculationCaseTransformation ccTrans = TransformationFactory
          .createTransformation( element );

      ccTrans.transform( targetFolder, new SubProgressMonitor( monitor, 1 ) );
    }
  }

  public String getCalcType()
  {
    final Modelspec modelspec = getModelspec();

    return modelspec.getTypeID();
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IResourceDelta delta = event.getDelta();
    final IResourceDelta metadataDelta = delta.findMember( getMetadataFile().getFullPath() );
    if( metadataDelta == null )
      return;

    switch( metadataDelta.getKind() )
    {
    case IResourceDelta.ADDED:
    case IResourceDelta.REMOVED:
    case IResourceDelta.CHANGED:
    {
      try
      {
        reloadMetadata();
      }
      catch( final CoreException e )
      {
        // TODO: error handling? -->> als job absetzen?
        e.printStackTrace();
      }
      break;
    }
    }
  }

  private void reloadMetadata() throws CoreException
  {
    try
    {
      m_metadata.clear();

      final IFile file = getMetadataFile();
      m_metadata.load( file.getContents() );
    }
    catch( final IOException e )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Error loading Metadata", e ) );
    }
  }

  public String[] getCalcCaseInputData( final IFolder folder, final IProgressMonitor monitor )
  {
    // modelspec holen
    final Modelspec modelspec = getModelspec();
    if( modelspec == null )
      return new String[] {};

    final Collection inputStrings = new ArrayList();
    try
    {
      // anhand der modelspec die dateien rausfinden
      final List inputList = modelspec.getInput();

      monitor.beginTask( "Eingabedateien werden gelesen", inputList.size() );

      // immer erst mal die .calculation Datei
      final IFile calcFile = folder.getFile( CALCULATION_FILE );
      inputStrings.add( loadFileIntoString( calcFile ) );

      for( final Iterator iter = inputList.iterator(); iter.hasNext(); )
      {
        final ModelspecType.InputType input = (ModelspecType.InputType)iter.next();
        final String path = input.getPath();

        final IFile file = input.isRelativeToCalcCase() ? folder.getFile( path ) : m_project
            .getFile( path );

        inputStrings.add( loadFileIntoString( file ) );

        monitor.worked( 1 );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }

    monitor.done();

    return (String[])inputStrings.toArray( new String[inputStrings.size()] );
  }

  private String loadFileIntoString( final IFile file ) throws UnsupportedEncodingException,
      CoreException, IOException
  {
    final BufferedReader br = new BufferedReader( new InputStreamReader( file.getContents(), file
        .getCharset() ) );
    final StringBuffer sb = new StringBuffer();
    while( br.ready() )
    {
      final String line = br.readLine();
      if( line == null )
        break;

      sb.append( line );
      sb.append( "\n" );
    }
    br.close();
    return sb.toString();
  }

  private Modelspec getModelspec()
  {
    try
    {
      final IFile file = m_project.getFile( MODELLTYP_MODELSPEC_XML );

      final ObjectFactory faktory = new ObjectFactory();
      final Unmarshaller unmarshaller = faktory.createUnmarshaller();
      return (Modelspec)unmarshaller.unmarshal( file.getContents() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      return null;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public IStatus putCalcCaseOutputData( final IFolder folder, final String[] results,
      final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Ergebnisdaten werden abgelegt", 2000 );

    final Modelspec modelspec = getModelspec();

    final List outputList = modelspec.getOutput();
    int count = 0;
    if( results == null || results.length != outputList.size() )
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Ergebnisdaten passen nicht zur Modellspezifikation", null );

    final IFolder resultsFolder = folder.getFolder( CALC_RESULT_FOLDER );
    if( resultsFolder.exists() )
      resultsFolder.delete( false, true, new SubProgressMonitor( monitor, 1000 ) );

    resultsFolder.create( false, true, null );

    for( Iterator iter = outputList.iterator(); iter.hasNext(); )
    {
      final ModelspecType.OutputType output = (ModelspecType.OutputType)iter.next();
      final String path = output.getPath();

      final IFile file = resultsFolder.getFile( path );

      file.create( new StringInputStream( results[count++] ), false, null );
    }

    monitor.worked( 1000 );

    return Status.OK_STATUS;
  }

  public static void runPrognose( final Shell shell, final String name )
  {
    try
    {
      final IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().findMember( name );

      final Wizard wizard = new Wizard()
      {
        public boolean performFinish()
        {
          // TODO: start calculation and show results
          return false;
        }
      };
      wizard.setWindowTitle( "Prognoserechnung für " + project.getName() );

      // wizard seiten hinzufügen!
      final IFile wizardConfigFile = (IFile)project.findMember( MODELLTYP_CALCWIZARD_XML );
      final InputSource inputSource = new InputSource( wizardConfigFile.getContents() );
      inputSource.setEncoding( wizardConfigFile.getCharset() );

      final Calcwizard calcwizardData = (Calcwizard)new ObjectFactory().createUnmarshaller()
          .unmarshal( inputSource );
      final List pages = calcwizardData.getPage();
      for( Iterator pIt = pages.iterator(); pIt.hasNext(); )
      {
        final CalcwizardType.PageType page = (CalcwizardType.PageType)pIt.next();

        final Properties props = new Properties();
        final List arglist = page.getArg();
        for( Iterator aIt = arglist.iterator(); aIt.hasNext(); )
        {
          final CalcwizardType.PageType.ArgType arg = (ArgType)aIt.next();
          props.setProperty( arg.getName(), arg.getValue() );
        }

        final String className = page.getClassName();
        final String pageTitle = page.getPageTitle();
        final String imageLocation = page.getImageLocation();
        final ImageDescriptor imageDesc = imageLocation == null ? null : ImageProvider.id( imageLocation );

        final IWizardPage wizardPage = (IWizardPage)ClassUtilities.newInstance( className, IWizardPage.class, ModelNature.class.getClassLoader(), new Object[] { project, pageTitle, imageDesc, props } );
        wizard.addPage( wizardPage );
      }

      final WizardDialog wizardDialog = new WizardDialog( shell, wizard );
      
      wizardDialog.create();
      wizardDialog.getShell().setBounds(shell.getBounds());
      wizardDialog.open();
    }
    catch( final ResourceException re )
    {
      re.printStackTrace();
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( SecurityException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( IllegalArgumentException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( ClassUtilityException e )
    {
      e.printStackTrace();
    }

    //MessageDialog.openInformation( shell, "Prognoserechnung",
    // project.getName() );
  }

}