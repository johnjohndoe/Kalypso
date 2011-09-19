package org.kalypso.kalypso1d2d.pjt.map;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.deegree.io.shpapi.shape_new.ShapeFile;
import org.deegree.io.shpapi.shape_new.ShapeFileReader;
import org.deegree.model.feature.FeatureCollection;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import au.com.bytecode.opencsv.CSVReader;

public class ImportHydrographWizard  extends Wizard implements IWorkbenchWizard
{
//  private final static String PROPERTY_MAX_PERC_RATE = Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.12" ); //$NON-NLS-1$

//  private final static String PROPERTY_GW_FACTOR = Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.14" ); //$NON-NLS-1$

  private ImportHydrographWizardPage m_wizardPage;
  
  private final int m_grabRadius = 10;

  private final IHydrographCollection m_hydrographCollection;

  private final IKalypsoFeatureTheme m_hydroTheme;

  private final IFEDiscretisationModel1d2d m_discModel;

  private File inFile = null;

  String m_errMsg = "";

  private char m_separator = ';';

  public ImportHydrographWizard( final IHydrographCollection hydrographCollection, final IKalypsoFeatureTheme hydroTheme, final IFEDiscretisationModel1d2d discModel )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.0" ) ); //$NON-NLS-1$
    m_hydrographCollection = hydrographCollection;
    m_hydroTheme = hydroTheme;
    
//    final String[] properties = new String[] { PROPERTY_MAX_PERC_RATE, PROPERTY_GW_FACTOR };
    m_wizardPage = new ImportHydrographWizardPage( "ImportHydrographsPage" ); //$NON-NLS-1$
    
    m_wizardPage.setDescription( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.2" ) ); //$NON-NLS-1$
    
    m_discModel = discModel;
    
    addPage( m_wizardPage );
  }

  private boolean doImportFromShape(){
    ShapeFileReader reader = new ShapeFileReader( inFile.getAbsolutePath() );
    ShapeFile sf;
    List< Feature > lListNewHydroFeatures = new ArrayList< Feature >();
    try
    {
      sf = reader.read();
      FeatureCollection fc = sf.getFeatureCollection();
      
        for( int i = 0; i < fc.size(); ++i )
        {
          org.deegree.model.feature.Feature lf = fc.getFeature( i );
          double lDoubleX;
          double lDoubleY; 
          try{
            lDoubleX = lf.getDefaultGeometryPropertyValue().getCentroid().getX();
            lDoubleY = lf.getDefaultGeometryPropertyValue().getCentroid().getY();
          }
          catch (Exception e) {
            e.printStackTrace();
            continue;
          }
          GM_Position hydroPositionFromElement = checkPositionOfNewHydrograph( lDoubleX, lDoubleY );
          lListNewHydroFeatures.add( createHydrograph( hydroPositionFromElement, lf.getId(), lf.getDescription() ).getFeature() );
        }
        postCommand( lListNewHydroFeatures.toArray( new Feature[ lListNewHydroFeatures.size() ] ) );
    }
    catch( Exception e )
    {
      m_errMsg += e.getMessage();
      return false;
    }
    return true;
    
  }
  
  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    inFile = m_wizardPage.getShapeFile(); 
    boolean bDone = true;
    if( inFile.getName().endsWith( ".shp" ) ){ //$NON-NLS-1$
      bDone = doImportFromShape();
    }
    else if( inFile.getName().endsWith( ".txt" ) || inFile.getName().endsWith( ".csv" ) ){ //$NON-NLS-1$
      bDone = doImportFromCsv();
    }
   
    if( !bDone ){
      return showErrorMsg( IStatus.CANCEL );
    }
    else if( m_errMsg != null && m_errMsg != "" ){
      return showErrorMsg( Status.WARNING );
    }
    return bDone;
  }

  private boolean doImportFromCsv( )
  {
    List< Feature > lListNewHydroFeatures = new ArrayList< Feature >();
    try
    {
      final CSVReader readerTimeSeries = new CSVReader( new FileReader( inFile ), m_separator  );
      String[] nextLine;
      while( (nextLine = readerTimeSeries.readNext()) != null )
      {
        final String lStrX = nextLine[0];
        final String lStrY = nextLine[1];
        double lDoubleX; 
        double lDoubleY; 
        try{
          lDoubleX = NumberUtils.parseDouble( lStrX );
          lDoubleY = NumberUtils.parseDouble( lStrY );
        }
        catch (NumberFormatException e) {
          e.printStackTrace();
          continue;
        }
        final String name = nextLine[2];
        String description = "";
        if( nextLine.length  == 4 ){
          description = nextLine[2];
        }
        GM_Position hydroPositionFromElement = checkPositionOfNewHydrograph( lDoubleX, lDoubleY );
        lListNewHydroFeatures.add( createHydrograph( hydroPositionFromElement, name, description ).getFeature() );
      }
      postCommand( lListNewHydroFeatures.toArray( new Feature[ lListNewHydroFeatures.size() ] ) );
    }
    catch( FileNotFoundException e )
    {
      m_errMsg += e.getMessage();
      e.printStackTrace();
      return false;
    }
    catch( IOException e )
    {
      m_errMsg += e.getMessage();
      e.printStackTrace();
      return false;
    }
    return true;
  }

  private GM_Position checkPositionOfNewHydrograph( final double lDoubleX, final double lDoubleY )
  {
    GM_Point gm_pos = GeometryFactory.createGM_Point( lDoubleX, lDoubleY, m_wizardPage.getSelectedCRS() );
    IFE1D2DNode node = m_discModel.findNode( gm_pos, m_grabRadius );
    if( node == null ){
      return null;
    }
    return node.getPoint().getPosition();
  }

  private boolean showErrorMsg( final int status )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    display.asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        final Shell shell = display.getActiveShell();
        IStatus lStatus = Status.CANCEL_STATUS;
        if( status != IStatus.CANCEL ){
          lStatus = Status.OK_STATUS;
        }
        ErrorDialog.openError( shell, "Import Hydrographs Warnings", m_errMsg, lStatus ); //$NON-NLS-1$
      }
    } );
    return true;
  }

  private IHydrograph createHydrograph( final GM_Position hydroPositionFromElement, final String name, final String description ){

    if( hydroPositionFromElement == null || m_hydrographCollection == null ){
      m_errMsg += "cannot find element for creation of hydrograph" + " cannot find any element on target position";
      return null;
    }
    IHydrograph existingHydrograph = m_hydrographCollection.findHydrograph( hydroPositionFromElement, 0.0 );
    if( existingHydrograph != null ){
      return null;
    }
    final CommandableWorkspace workspace = m_hydroTheme.getWorkspace();

    final Feature parentFeature = m_hydrographCollection.getFeature();
    final IRelationType parentRelation = m_hydrographCollection.getWrappedList().getParentFeatureTypeProperty();
    final IHydrograph hydro = HydrographUtils.createNewHydrographFeature( workspace, parentFeature, parentRelation, name, description );
    
    if( hydro == null )
    {
      m_errMsg += "cannot create a new hydrograph";
      return null;
    }
    
    
    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    hydro.setLocation( GeometryFactory.createGM_Point( hydroPositionFromElement, crs ) );
    
    return hydro;

  }

  private void postCommand( final Feature[] newHydroFeatures )
  {
//    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, newHydroFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    /* Post it as an commands */
    final CommandableWorkspace workspace = m_hydroTheme.getWorkspace();
    final IFeatureSelectionManager selectionManager = m_hydroTheme.getSelectionManager();
    final Feature parentFeature = m_hydrographCollection.getFeature();
    final IRelationType parentRelation = m_hydrographCollection.getWrappedList().getParentFeatureTypeProperty();

    final Display display = PlatformUI.getWorkbench().getDisplay();
    for( final Feature lf: newHydroFeatures ){
      final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, lf, selectionManager, true, true );
      try
      {
        workspace.postCommand( command );
      }
      catch( final Throwable e )
      {
        m_errMsg += Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.3" + e.getMessage() );
      }
      
    }
    /*
    final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, hydro.getFeature(), selectionManager, true, true );
    display.asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        try
        {
          workspace.postCommand( command );
        }
        catch( final Throwable e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          final Shell shell = display.getActiveShell();
          m_errMsg = Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.3" );
//          ErrorDialog.openError( shell, getName(), ), status ); //$NON-NLS-1$
        }
      }
    } );
    */
  }
  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    // TODO Auto-generated method stub
    
  }

  public String getErrMsg( )
  {
    return m_errMsg;
  }
  
  
}
