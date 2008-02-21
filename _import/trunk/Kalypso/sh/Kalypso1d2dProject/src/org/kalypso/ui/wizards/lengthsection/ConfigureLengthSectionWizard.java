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
package org.kalypso.ui.wizards.lengthsection;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class ConfigureLengthSectionWizard extends Wizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults";

  private final static String PAGE_GENERATE_LENGTH_SECTION_NAME = "generateLengthSection";

  private final IScenarioResultMeta m_resultModel;

  private final IFolder m_scenarioFolder;

  private IFile m_selectedResultFile;

  private final MapPanel m_mapPanel;

  public ConfigureLengthSectionWizard( IFolder scenarioFolder, final IScenarioResultMeta resultModel, MapPanel mapPanel )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    m_mapPanel = mapPanel;
    setWindowTitle( "1D2D-Ergebnisse" );

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    // define properties page ((choose existing themes (lines) / river name field selection combo river name selection
    // combo, delta station spinner, station field select combo)
    final ConfigureLengthSectionWizardPage lengthSectionPage = new ConfigureLengthSectionWizardPage( PAGE_GENERATE_LENGTH_SECTION_NAME, "Längsschnitteinstellungen", null, m_mapPanel );

    // select time step page
    final DocumentResultViewerFilter resultFilter = new DocumentResultViewerFilter();
    final Result1d2dMetaComparator comparator = new Result1d2dMetaComparator();

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, "Ergebnis für Längsschnitt auswählen", null, resultFilter, comparator, null );

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( lengthSectionPage );
    addPage( selectResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final ConfigureLengthSectionWizardPage parameterPage = (ConfigureLengthSectionWizardPage) getPage( PAGE_GENERATE_LENGTH_SECTION_NAME );
    final LengthSectionParameters lengthSectionParameters = parameterPage.getLengthSectionParameters();
    final SelectResultWizardPage resultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = resultPage.getSelectedResults();

    if( results.length == 0 )
    {
      MessageDialog.openInformation( getShell(), "Kein Ergebnis ausgewählt", "Bitte wählen Sie einen Zeitschritt aus, für den der Längsschnitt generiert werden soll." );
      return false;
    }

    /* Start */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "Erzeuge Längsschnitt: ", 12 );

        try
        {
          monitor.subTask( "...erzeuge Stationseinträge..." );
          BigDecimal[] stationList = lengthSectionParameters.getStationList();

          if( stationList == null )
            return StatusUtilities.createErrorStatus( "Längsschnitt konnte nicht erzeugt werden:  Keine Stationseinträge vorhanden (falsche Gewässerlinie?)" );

          monitor.worked( 3 );

          // get the Observation Template from the resources
          final URL lsObsUrl = ConfigureLengthSectionWizard.class.getResource( "resources/lengthSectionTemplate.gml" );
          final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( lsObsUrl, null );
          final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );

          // just get the first selected item
          IResultMeta resultMeta = results[0];

          /* get the result data */

          if( resultMeta instanceof IStepResultMeta )
          {
            IStepResultMeta stepResult = (IStepResultMeta) resultMeta;

            IFeatureWrapperCollection<IResultMeta> children = stepResult.getChildren();
            for( IResultMeta child : children )
            {
              // get all documents
              if( child instanceof IDocumentResultMeta )
              {
                IDocumentResultMeta docResult = (IDocumentResultMeta) child;
                DOCUMENTTYPE documentType = docResult.getDocumentType();

                GM_TriangulatedSurface surface = null;

                if( documentType == DOCUMENTTYPE.tinWsp )
                {
                  monitor.subTask( "...lese Wasserspiegel aus..." );
                  surface = getSurface( docResult );
                  if( surface != null )
                  {
                    LengthSectionHandler2d handler1 = new LengthSectionHandler2d( lsObs, surface, lengthSectionParameters.getRiverFeatures(), stationList, documentType );
                  }
                  else
                    return StatusUtilities.createErrorStatus( "Längsschnitt konnte nicht erzeugt werden:  Wasserspiegellagen konnten nicht geladen werden." );
                }
              }
            }

            monitor.worked( 4 );

            IResultMeta parent = stepResult.getParent();
            if( parent instanceof ICalcUnitResultMeta )
            {
              ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) parent;
              IFeatureWrapperCollection<IResultMeta> calcUniChildren = calcUnitResult.getChildren();
              for( IResultMeta child : calcUniChildren )
              {
                if( child instanceof IDocumentResultMeta )
                {
                  IDocumentResultMeta docResult = (IDocumentResultMeta) child;
                  DOCUMENTTYPE documentType = docResult.getDocumentType();

                  GM_TriangulatedSurface surface = null;

                  if( documentType == DOCUMENTTYPE.tinTerrain )
                  {
                    monitor.subTask( "...lese Sohllagen aus..." );
                    surface = getSurface( docResult );
                    if( surface != null )
                    {
                      LengthSectionHandler2d handler1 = new LengthSectionHandler2d( lsObs, surface, lengthSectionParameters.getRiverFeatures(), stationList, documentType );
                    }
                    else
                      return StatusUtilities.createErrorStatus( "Längsschnitt konnte nicht erzeugt werden:  Modelldaten konnten nicht geladen werden." );
                  }
                }
              }
            }

            monitor.worked( 4 );

            /* write the observation gml */
            if( lsObs.getResult().size() > 0 )
            {
              monitor.subTask( "...exportiere Längsschnittdaten..." );
              ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );

              IPath docPath = resultMeta.getFullPath();
              IFolder folder = m_scenarioFolder.getFolder( docPath );

              IFile lsFile = folder.getFile( "lengthSection.gml" );

              /* delete the existing lengthsection file */
              if( lsFile.exists() )
              {
                lsFile.delete( true, true, new NullProgressMonitor() );
              }

              File lsObsFile = lsFile.getLocation().toFile();

              GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" );

              // TODO: if there is already a length section document, delete it
              for( IResultMeta child : children )
              {
                if( child instanceof IDocumentResultMeta )
                {
                  IDocumentResultMeta docResult = (IDocumentResultMeta) child;
                  if( docResult.getDocumentType() == DOCUMENTTYPE.lengthSection )
                  {
                    stepResult.removeChild( docResult );
                    break;
                  }
                }
              }
              BigDecimal min = new BigDecimal( 0 );
              BigDecimal max = new BigDecimal( 0 );
              ResultMeta1d2dHelper.addDocument( stepResult, "Längsschnitt", "2d-Längsschnitt", IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( "lengthSection.gml" ), Status.OK_STATUS, min, max );
            }
            monitor.worked( 1 );
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
          return StatusUtilities.statusFromThrowable( e, "Konnte Längsschnitt nicht erzeugen." );
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
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler bei Längsschnitterzeugung", status );

    return !status.matches( IStatus.ERROR );

  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;

  }

  private GM_TriangulatedSurface getSurface( IDocumentResultMeta docResult )
  {
    GM_TriangulatedSurface surface = null;

    IPath docPath = docResult.getFullPath();
    IFolder folder = m_scenarioFolder.getFolder( docPath );

    try
    {
      final URL surfaceURL = ResourceUtilities.createURL( folder );
      GMLWorkspace w = GmlSerializer.createGMLWorkspace( surfaceURL, null );

      final String targetCRS = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

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
    return surface;
  }
}
