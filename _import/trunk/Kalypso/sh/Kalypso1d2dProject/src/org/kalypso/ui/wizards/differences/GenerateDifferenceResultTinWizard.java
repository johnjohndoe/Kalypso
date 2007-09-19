/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.wizards.differences;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import javax.activation.UnsupportedDataTypeException;
import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.TriangulatedSurfaceTriangleEater;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.differences.IMathOperatorDelegate.MATH_OPERATOR;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.ThemeConstructionFactory;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypso.ui.wizards.results.filters.NonMapDataResultViewerFilter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class GenerateDifferenceResultTinWizard extends Wizard
{
  private static final String PAGE_SELECT_DESTINATION_RESULTS_NAME = "selectDestinationResults";

  private static final String PAGE_SELECT_MASTER_RESULTS_NAME = "selectMasterResults";

  private static final String PAGE_SELECT_SLAVE_RESULTS_NAME = "selectSlaveResults";

  private final IScenarioResultMeta m_resultModel;

  private final IFolder m_scenarioFolder;

  private IFile m_selectedResultFile;

  public GenerateDifferenceResultTinWizard( IFolder scenarioFolder, final IScenarioResultMeta resultModel )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    setWindowTitle( "1D2D-Ergebnisse" );

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {

    // select master document page
    final NonMapDataResultViewerFilter resultFilter = new NonMapDataResultViewerFilter();
    final SelectResultWizardPage selectMasterResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_MASTER_RESULTS_NAME, "Ergebnis-Minuend auswählen", null, resultFilter, null );
    final SelectResultWizardPage selectSlaveResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_SLAVE_RESULTS_NAME, "Ergebnis-Subtrahend auswählen", null, resultFilter, null );

    final DocumentResultViewerFilter resultFilter2 = new DocumentResultViewerFilter();
    final ThemeConstructionFactory themeConstructionFactory = new ThemeConstructionFactory( m_scenarioFolder );
    final SelectResultWizardPage selectDestinationResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_DESTINATION_RESULTS_NAME, "Ziel auswählen", null, resultFilter2, themeConstructionFactory );

    selectMasterResultWizardPage.setResultMeta( m_resultModel );
    selectSlaveResultWizardPage.setResultMeta( m_resultModel );
    selectDestinationResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectMasterResultWizardPage );
    addPage( selectSlaveResultWizardPage );
    addPage( selectDestinationResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {

    final MATH_OPERATOR operator = IMathOperatorDelegate.MATH_OPERATOR.eMinus;

    final SelectResultWizardPage masterResultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_MASTER_RESULTS_NAME );
    final IResultMeta[] masterResults = masterResultPage.getSelectedResults();

    final GM_TriangulatedSurface[] surfaces = new GM_TriangulatedSurface[2];

    if( masterResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), "Kein Ergebnis-Minuend ausgewählt", "Bitte wählen Sie ein Ergebnis aus, von dem Sie andere Ergbnisdaten abziehen wollen." );
      return false;
    }

    final SelectResultWizardPage slaveResultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_SLAVE_RESULTS_NAME );
    final IResultMeta[] slaveResults = slaveResultPage.getSelectedResults();

    if( slaveResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), "Kein Ergebnis-Subtrahend ausgewählt", "Bitte wählen Sie ein Ergebnis aus, das abgezogen wird." );
      return false;
    }

    final SelectResultWizardPage destinationResultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_DESTINATION_RESULTS_NAME );
    final IResultMeta[] destinationResults = destinationResultPage.getSelectedResults();

    if( destinationResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), "Kein Ziel ausgewählt", "Bitte wählen Sie ein Ziel aus, unter dem die erzeugten Differenzen abgelegt werden sollen." );
      return false;
    }

    /* Start */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "Erzeuge Differenzen: ", 10 );

        try
        {
          GM_TriangulatedSurface masterSurface = null;
          GM_TriangulatedSurface slaveSurface = null;

          monitor.subTask( "...lese Minuend..." );
          masterSurface = getSurfaceData( masterResults[0] );
          monitor.worked( 3 );
          if( masterSurface == null )
            return StatusUtilities.createErrorStatus( "Differenzen konnte nicht erzeugt werden:  Minuend konnte nicht gelesen werden." );

          monitor.subTask( "...lese Subtrahent..." );
          slaveSurface = getSurfaceData( slaveResults[0] );
          monitor.worked( 3 );
          if( slaveSurface == null )
            return StatusUtilities.createErrorStatus( "Differenzen konnte nicht erzeugt werden:  Subtrahent konnte nicht gelesen werden." );

          // just get the first selected items
          surfaces[0] = masterSurface;
          surfaces[1] = slaveSurface;
          final IResultMeta destResult = destinationResults[0].getParent();

          monitor.subTask( "...generiere Differenzen..." );

          // TODO: where to write the differences? (scenario diff folder / everywhere)
          // why do we receive here only document results??
          IPath docPath = destResult.getFullPath();
          IFolder folder = m_scenarioFolder.getFolder( docPath );

          // TODO: generate unique name for difference file!!
          String name = "tin";
          String extension = ".gml";
          File parentDir = docPath.toFile();
          String uniqueFileName = FileUtilities.createNewUniqueFileName( name, extension, parentDir );

          IFile destFile = folder.getFile( uniqueFileName );
          IStatus status = generateDifferences( surfaces, operator, destFile );
          monitor.worked( 3 );

          /* update the result db */
          if( status.isOK() )
          {
            Path path = new Path( destFile.getName() );

            BigDecimal min = new BigDecimal( 0 );
            BigDecimal max = new BigDecimal( 0 );

            if( destResult instanceof IStepResultMeta )
            {
              IStepResultMeta stepResult = (IStepResultMeta) destResult;
              stepResult.addDocument( "Differenzen", "2d-Differenzen", IDocumentResultMeta.DOCUMENTTYPE.tinDifference, path, Status.OK_STATUS, min, max );
            }
            else if( destResult instanceof ICalcUnitResultMeta )
            {
              ICalcUnitResultMeta calcResult = (ICalcUnitResultMeta) destResult;
              calcResult.addDocument( "Differenzen", "2d-Differenzen", IDocumentResultMeta.DOCUMENTTYPE.tinDifference, path, Status.OK_STATUS, min, max );
            }
            else if( destResult instanceof IScenarioResultMeta )
            {
              IStepResultMeta scenResult = (IStepResultMeta) destResult;
              scenResult.addDocument( "Differenzen", "2d-Differenzen", IDocumentResultMeta.DOCUMENTTYPE.tinDifference, path, Status.OK_STATUS, min, max );
            }
            else
            {
              throw new UnsupportedDataTypeException( "Daten können nicht an bestehende Ergebnis-Dokumente geschrieben werden." );
              // TODO: cleanFiles();
            }
          }
          else
            return status;

          monitor.worked( 1 );

        }
        catch( Exception e )
        {
          e.printStackTrace();
          return StatusUtilities.statusFromThrowable( e, "Konnte Differenzen nicht erzeugen." );
        }
        catch( final Throwable t )
        {
          throw new InvocationTargetException( t );
        }
        finally
        {
          monitor.done();
        }
        return StatusUtilities.createOkStatus( "Längsschnitt erzeugt." );

      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler bei Differenzenerzeugung", status );

    return !status.matches( IStatus.ERROR );

  }

  protected IStatus generateDifferences( GM_TriangulatedSurface[] surfaces, MATH_OPERATOR operator, IFile diffFile )
  {
    // TODO: generate unique name for difference file!!

    final CS_CoordinateSystem crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

    final File tinResultFile = diffFile.getLocation().toFile();

    try
    {
      final GMLWorkspace triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ), tinResultFile.toURL(), null );
      final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( crs );
      final Feature triangleFeature = triangleWorkspace.getRootFeature();
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ), surface );

      // Loop over master triangles
      final GM_TriangulatedSurface masterSurface = surfaces[0];
      final GM_TriangulatedSurface slaveSurface = surfaces[1];

      TriangulatedSurfaceTriangleEater eater = new TriangulatedSurfaceTriangleEater( tinResultFile, triangleWorkspace, surface, ResultType.TYPE.DIFFERENCE );

      // TODO: use monitor to display progress.

      for( GM_Triangle triangle : masterSurface )
      {
        final List<GM_Point> nodeList = new LinkedList<GM_Point>();

        GM_Position[] ring = triangle.getExteriorRing();

        for( int i = 0; i < ring.length - 1; i++ )
        {
          final GM_Point point = GeometryFactory.createGM_Point( ring[i], crs );

          final double o1 = point.getZ();
          final double o2 = slaveSurface.getValue( point );

          BigDecimal result = null;

          if( !Double.isNaN( o2 ) )
          {
            result = operator.getOperator().getResult( new BigDecimal( o1 ), new BigDecimal( o2 ) );
          }

          if( result != null )
          {
            final GM_Point newPoint = GeometryFactory.createGM_Point( point.getX(), point.getY(), result.doubleValue(), crs );
            nodeList.add( newPoint );
          }
        }

        if( nodeList.size() == 3 )
          eater.add( nodeList );
      }

      eater.finished();
      return Status.OK_STATUS;
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Konnte Differenzen nicht erzeugen." );
    }
    catch( InvocationTargetException e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Konnte Differenzen nicht erzeugen." );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Konnte Differenzen nicht erzeugen." );
    }
  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;
  }

  private GM_TriangulatedSurface getSurfaceData( final IResultMeta resultMeta )
  {
    /* get the result data */

    if( resultMeta instanceof IDocumentResultMeta )
    {
      IDocumentResultMeta docResult = (IDocumentResultMeta) resultMeta;

      DOCUMENTTYPE documentType = docResult.getDocumentType();

      if( documentType == DOCUMENTTYPE.tinWsp || documentType == DOCUMENTTYPE.tinDepth || documentType == DOCUMENTTYPE.tinVelo || documentType == DOCUMENTTYPE.tinShearStress )
      {
        try
        {
          IPath docPath = docResult.getFullPath();
          IFolder folder = m_scenarioFolder.getFolder( docPath );

          final URL surfaceURL = ResourceUtilities.createURL( folder );
          GMLWorkspace w = GmlSerializer.createGMLWorkspace( surfaceURL, null );

          final CS_CoordinateSystem targetCRS = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

          w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

          GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryProperty();

          if( geometryProperty instanceof GM_TriangulatedSurface )
          {
            return (GM_TriangulatedSurface) geometryProperty;
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        return null;
      }
    }
    return null;
  }

}
