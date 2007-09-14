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
import java.math.BigDecimal;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.KalypsoCorePlugin;
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
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

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
    final StepResultViewerFilter resultFilter = new StepResultViewerFilter();
    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, "Ergebnis für Längsschnitt auswählen", null, resultFilter, null );

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
    LengthSectionParameters lengthSectionParameters = parameterPage.getLengthSectionParameters();

    try
    {
      // get the Observation Template from the resources
      final URL lsObsUrl = ConfigureLengthSectionWizard.class.getResource( "resources/lengthSectionTemplate.gml" );
      final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( lsObsUrl, null );
      final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );

      final SelectResultWizardPage resultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_RESULTS_NAME );
      final IResultMeta[] results = resultPage.getSelectedResults();

      // just get the first selected item
      IResultMeta resultMeta = results[0];

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
              surface = getSurface( docResult );
              if( surface != null )
              {
                LengthSectionHandler2d handler1 = new LengthSectionHandler2d( lsObs, surface, lengthSectionParameters.getRiverFeatures(), lengthSectionParameters.getStationList(), documentType );
              }
            }
          }
        }

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
                surface = getSurface( docResult );
                if( surface != null )
                {
                  LengthSectionHandler2d handler1 = new LengthSectionHandler2d( lsObs, surface, lengthSectionParameters.getRiverFeatures(), lengthSectionParameters.getStationList(), documentType );
                }
              }
            }
          }
        }

        /* write the observation gml */
        if( lsObs.getResult().size() > 0 )
        {
          ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );

          IPath docPath = resultMeta.getFullPath();
          IFolder folder = m_scenarioFolder.getFolder( docPath );

          IFile lsFile = folder.getFile( "lengthSection.gml" );

          File lsObsFile = lsFile.getLocation().toFile();

          GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" );

          // TODO: if there is already a length section document, delete it

          BigDecimal min = new BigDecimal( 0 );
          BigDecimal max = new BigDecimal( 0 );
          stepResult.addDocument( "Längsschnitt", "2d-Längsschnitt", IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( "lengthSection.gml" ), Status.OK_STATUS, min, max );

        }
      }

    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      // TODO: file handling
      return false;
    }

    return true;

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
    return surface;
  }
}
