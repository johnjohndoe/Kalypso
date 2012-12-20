package org.kalypso.kalypso1d2d.pjt.map;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
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
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import au.com.bytecode.opencsv.CSVReader;

public class ImportHydrographWizard extends Wizard implements IWorkbenchWizard
{
  private final ImportHydrographWizardPage m_wizardPage;

  private final int m_grabRadius = 10;

  private final IHydrographCollection m_hydrographCollection;

  private final IKalypsoFeatureTheme m_hydroTheme;

  private final IFEDiscretisationModel1d2d m_discModel;

  private File m_inFile = null;

  String m_errMsg = ""; //$NON-NLS-1$

  private final char m_defaultSeparator = ';';

  private char m_separator = m_defaultSeparator;

  private final Set<String> m_setNewCreatedNames = new HashSet<>();

  public ImportHydrographWizard( final IHydrographCollection hydrographCollection, final IKalypsoFeatureTheme hydroTheme, final IFEDiscretisationModel1d2d discModel )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.0" ) ); //$NON-NLS-1$
    m_hydrographCollection = hydrographCollection;
    m_hydroTheme = hydroTheme;

    m_wizardPage = new ImportHydrographWizardPage( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.0" ) ); //$NON-NLS-1$

    m_wizardPage.setDescription( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.2" ) ); //$NON-NLS-1$

    m_discModel = discModel;

    addPage( m_wizardPage );
  }

  private boolean doImportFromShape( )
  {
    try( ShapeFile shapeFile = new ShapeFile( m_inFile.getAbsolutePath(), Charset.defaultCharset(), FileMode.READ ) )
    {
      String shapeSRS = m_wizardPage.getSelectedCRS();

      final List<Feature> lListNewHydroFeatures = new ArrayList<>();

      final int numRecords = shapeFile.getNumRecords();
      for( int i = 0; i < numRecords; i++ )
      {
        try
        {
          final ISHPGeometry shape = shapeFile.getShape( i );
          final GM_Object gmObject = SHP2GM_Object.transform( shapeSRS, shape );

          final double lDoubleX = gmObject.getCentroid().getX();
          final double lDoubleY = gmObject.getCentroid().getY();

          final GM_Position hydroPositionFromElement = checkPositionOfNewHydrograph( lDoubleX, lDoubleY );
          if( hydroPositionFromElement == null )
            continue;

          final String lStrName = getNextName( "Hydrograph" ); //$NON-NLS-1$
          final IHydrograph hydrograph = createHydrograph( hydroPositionFromElement, lStrName, StringUtils.EMPTY );
          lListNewHydroFeatures.add( hydrograph );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          continue;
        }
      }

      postCommand( lListNewHydroFeatures.toArray( new Feature[lListNewHydroFeatures.size()] ) );
      return true;
    }
    catch( final Exception e )
    {
      m_errMsg += e.getMessage();
      return false;
    }
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    m_setNewCreatedNames.clear();
    m_inFile = m_wizardPage.getShapeFile();
    boolean bDone = true;
    if( m_inFile.getName().endsWith( ".shp" ) ) { //$NON-NLS-1$
      bDone = doImportFromShape();
    }
    else if( m_inFile.getName().endsWith( ".csv" ) ) { //$NON-NLS-1$
      m_separator = getSeparatorAsChar( m_wizardPage.getsSeparator() );
      bDone = doImportFromCsv();
    }
    else if( m_inFile.getName().endsWith( ".txt" ) ) { //$NON-NLS-1$
      m_separator = getSeparatorAsChar( m_wizardPage.getsSeparator() );
      bDone = doImportFromTxt();
    }

    if( !bDone )
    {
      return showErrorMsg( IStatus.CANCEL );
    }
    else if( m_errMsg != null && m_errMsg != "" ) { //$NON-NLS-1$
      return showErrorMsg( Status.WARNING );
    }
    return bDone;
  }

  private char getSeparatorAsChar( final String separator )
  {
    if( separator != null )
    {
      final String lStrSep = separator.trim();
      if( lStrSep.charAt( 0 ) == '\\' )
      {
        if( lStrSep.charAt( 1 ) == 't' )
        {
          return '\t';
        }
        else if( lStrSep.charAt( 1 ) == '0' )
        {
          return '\0';
        }
        else if( lStrSep.charAt( 1 ) == 'r' )
        {
          return '\r';
        }
        else if( lStrSep.charAt( 1 ) == 'n' )
        {
          return '\n';
        }
      }
      else if( lStrSep.length() == 1 )
      {
        return lStrSep.charAt( 0 );
      }
    }
    return m_defaultSeparator;
  }

  private boolean doImportFromCsv( )
  {
    final List<Feature> lListNewHydroFeatures = new ArrayList<>();

    try( final CSVReader readerTimeSeries = new CSVReader( new FileReader( m_inFile ), m_separator ) )
    {
      String[] nextLine;
      while( (nextLine = readerTimeSeries.readNext()) != null )
      {
        final String lStrX = nextLine[0];
        final String lStrY = nextLine[1];
        double lDoubleX;
        double lDoubleY;
        try
        {
          lDoubleX = NumberUtils.parseDouble( lStrX );
          lDoubleY = NumberUtils.parseDouble( lStrY );
        }
        catch( final NumberFormatException e )
        {
          m_errMsg += e.getMessage();
          continue;
        }
        final String name = getNextName( nextLine[2] );
        String description = ""; //$NON-NLS-1$
        if( nextLine.length == 4 )
        {
          description = nextLine[2];
        }
        final GM_Position hydroPositionFromElement = checkPositionOfNewHydrograph( lDoubleX, lDoubleY );
        if( hydroPositionFromElement == null )
        {
          continue;
        }
        lListNewHydroFeatures.add( createHydrograph( hydroPositionFromElement, name, description ) );
      }
      postCommand( lListNewHydroFeatures.toArray( new Feature[lListNewHydroFeatures.size()] ) );
    }
    catch( final IOException | GeoTransformerException e )
    {
      m_errMsg += e.getMessage();
      e.printStackTrace();
      return false;
    }
    return true;
  }

  private boolean doImportFromTxt( )
  {
    final List<Feature> lListNewHydroFeatures = new ArrayList<>();

    try( final CSVReader readerTimeSeries = new CSVReader( new FileReader( m_inFile ), m_separator ) )
    {
      String[] nextLine;
      int lIntCounter = 0;
      int lIntPosColumnNr = -1;
      String name = m_inFile.getName().substring( 0, m_inFile.getName().length() - 4 );
      final int lIntExportSuffixPos = m_inFile.getName().indexOf( ExportHydrographWizard.EXPORT_FILE_NAME_SUFFIX );
      if( lIntExportSuffixPos > -1 )
      {
        name = m_inFile.getName().substring( 0, lIntExportSuffixPos );
      }
      while( (nextLine = readerTimeSeries.readNext()) != null )
      {
        // System.out.println( Arrays.asList( nextLine ) );
        if( nextLine.length == 0 || nextLine.length == 1 && "".equals( nextLine[0].trim() ) ) { //$NON-NLS-1$
          continue;
        }
        if( lIntCounter == 0 )
        {
          lIntPosColumnNr = tryToParseHeaderFromKalypsoExport( nextLine );
          lIntCounter++;
          continue;
        }

        final String lStrXY = nextLine[lIntPosColumnNr];
        double lDoubleX;
        double lDoubleY;
        try
        {
          final int lPosSepLocal = lStrXY.indexOf( ' ' );
          lDoubleX = NumberUtils.parseDouble( lStrXY.substring( 1, lPosSepLocal ) );
          lDoubleY = NumberUtils.parseDouble( lStrXY.substring( lPosSepLocal + 1, lStrXY.length() - 1 ) );
        }
        catch( final NumberFormatException e )
        {
          m_errMsg += e.getMessage();
          continue;
        }
        String description = ""; //$NON-NLS-1$
        if( nextLine.length == 4 )
        {
          description = nextLine[2];
        }
        final GM_Position hydroPositionFromElement = checkPositionOfNewHydrograph( lDoubleX, lDoubleY );
        if( hydroPositionFromElement == null )
        {
          continue;
        }
        final String nameAct = getNextName( name );

        lListNewHydroFeatures.add( createHydrograph( hydroPositionFromElement, nameAct, description ) );
        lIntCounter++;

      }
      postCommand( lListNewHydroFeatures.toArray( new Feature[lListNewHydroFeatures.size()] ) );
    }
    catch( IOException | GeoTransformerException e )
    {
      m_errMsg += e.getMessage();
      e.printStackTrace();
      return false;
    }
    return true;
  }

  private String getNextName( final String name )
  {
    if( !nameExists( name ) )
    {
      m_setNewCreatedNames.add( name );
      return name;
    }
    String lStrNameRes = name;
    int lIntAmountExistingHydro = m_hydrographCollection.getHydrographs().size();
    do
    {
      lStrNameRes = name + "_" + lIntAmountExistingHydro++; //$NON-NLS-1$
    }
    while( nameExists( lStrNameRes ) );
    m_setNewCreatedNames.add( lStrNameRes );
    return lStrNameRes;
  }

  boolean nameExists( final String name )
  {
    if( name == null )
      return true;
    if( m_setNewCreatedNames.contains( name ) )
    {
      return true;
    }
    for( final Object element : m_hydrographCollection.getHydrographs() )
    {
      final IHydrograph type = (IHydrograph)element;
      if( type.getName().trim().equalsIgnoreCase( name.trim() ) )
      {
        return true;
      }
    }
    return false;
  }

  /**
   *
   */
  private int tryToParseHeaderFromKalypsoExport( final String[] nextLine )
  {
    for( int i = 0; i < nextLine.length; i++ )
    {
      if( ExportHydrographWizard.COL_POS_NAME.equalsIgnoreCase( nextLine[i] ) )
      {
        return i;
      }
    }
    return -1;
  }

  private GM_Position checkPositionOfNewHydrograph( final double lDoubleX, final double lDoubleY ) throws GeoTransformerException
  {
    final GM_Point pos = GeometryFactory.createGM_Point( lDoubleX, lDoubleY, m_wizardPage.getSelectedCRS() );

    final GM_Point transformedPos = (GM_Point)pos.transform( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

    final IFE1D2DNode node = m_discModel.findNode( transformedPos, m_grabRadius );
    if( node == null )
      return null;

    GM_Position nodePosition = node.getPoint().getPosition();
    final IHydrograph existingHydrograph = m_hydrographCollection.findHydrograph( nodePosition, 0.01 );
    if( existingHydrograph != null )
      return null;

    return nodePosition;
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
        if( status != IStatus.CANCEL )
        {
          lStatus = Status.OK_STATUS;
        }
        ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.4" ), m_errMsg, lStatus ); //$NON-NLS-1$

      }
    } );
    return true;
  }

  private IHydrograph createHydrograph( final GM_Position hydroPositionFromElement, final String name, final String description )
  {
    if( hydroPositionFromElement == null || m_hydrographCollection == null )
    {
      m_errMsg += Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.4" );// ""; //$NON-NLS-1$
      return null;
    }
    final CommandableWorkspace workspace = m_hydroTheme.getWorkspace();

    final Feature parentFeature = m_hydrographCollection;
    final IRelationType parentRelation = m_hydrographCollection.getHydrographs().getFeatureList().getPropertyType();
    final IHydrograph hydro = HydrographUtils.createNewHydrographFeature( workspace, parentFeature, parentRelation, name, description );

    if( hydro == null )
    {
      m_errMsg += "failed to create new hydrograph"; //$NON-NLS-1$
      return null;
    }

    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    hydro.setLocation( GeometryFactory.createGM_Point( hydroPositionFromElement, crs ) );

    return hydro;

  }

  private void postCommand( final Feature[] newHydroFeatures )
  {
    // workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, newHydroFeatures,
    // FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    /* Post it as an commands */
    final CommandableWorkspace workspace = m_hydroTheme.getWorkspace();
    final IFeatureSelectionManager selectionManager = m_hydroTheme.getSelectionManager();
    final Feature parentFeature = m_hydrographCollection;
    final IRelationType parentRelation = m_hydrographCollection.getHydrographs().getFeatureList().getPropertyType();

    for( final Feature lf : newHydroFeatures )
    {
      final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, lf, selectionManager, true, true );
      try
      {
        workspace.postCommand( command );
      }
      catch( final Throwable e )
      {
        m_errMsg += Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.3" + e.getMessage() );//$NON-NLS-1$
      }

    }
    /*
     * final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1,
     * hydro.getFeature(), selectionManager, true, true ); display.asyncExec( new Runnable() {
     * @Override public void run( ) { try { workspace.postCommand( command ); } catch( final Throwable e ) { final
     * IStatus status = StatusUtilities.statusFromThrowable( e ); final Shell shell = display.getActiveShell(); m_errMsg
     * = Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographIxportWizard.3" ); // ErrorDialog.openError(
     * shell, getName(), ), status ); //$NON-NLS-1$ } } } );
     */
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
  }

  public String getErrMsg( )
  {
    return m_errMsg;
  }

}
