/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionParameters;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypsodeegree.KalypsoDeegreePlugin;
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
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults"; //$NON-NLS-1$

  private final static String PAGE_GENERATE_LENGTH_SECTION_NAME = "generateLengthSection"; //$NON-NLS-1$

  private final IScenarioResultMeta m_resultModel;

  private final IFolder m_scenarioFolder;

  private IFile m_selectedResultFile;

  private final IMapPanel m_mapPanel;

  private ConfigureLengthSectionWizardPage m_lengthSectionPage;

  public ConfigureLengthSectionWizard( final IFolder scenarioFolder, final IScenarioResultMeta resultModel, final IMapPanel mapPanel )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    m_mapPanel = mapPanel;
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.Title" ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_lengthSectionPage = new ConfigureLengthSectionWizardPage( PAGE_GENERATE_LENGTH_SECTION_NAME, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.3" ), null, m_mapPanel ); //$NON-NLS-1$
    // select time step page
    final DocumentResultViewerFilter resultFilter = new DocumentResultViewerFilter();
    final Result1d2dMetaComparator comparator = new Result1d2dMetaComparator();

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.4" ), null, resultFilter, comparator, null, null ); //$NON-NLS-1$

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( m_lengthSectionPage );
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
      MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.5" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }

    /* Start */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.7" ), 12 ); //$NON-NLS-1$

        try
        {
          monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.8" ) ); //$NON-NLS-1$
          final BigDecimal[] stationList = lengthSectionParameters.getStationList();

          if( stationList == null )
            return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.9" ) ); //$NON-NLS-1$

          monitor.worked( 3 );

          // get the Observation Template from the resources
          final URL lsObsUrl = LengthSectionHandler2d.class.getResource( "resources/lengthSectionTemplate.gml" ); //$NON-NLS-1$
          final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( lsObsUrl, null );
          final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );

          // just get the first selected item
          final IResultMeta resultMeta = results[0];

          /* get the result data */

          if( resultMeta instanceof IStepResultMeta )
          {
            final IStepResultMeta stepResult = (IStepResultMeta) resultMeta;

            final IFeatureWrapperCollection<IResultMeta> children = stepResult.getChildren();
            for( final IResultMeta child : children )
            {
              // get all documents
              if( child instanceof IDocumentResultMeta )
              {
                final IDocumentResultMeta docResult = (IDocumentResultMeta) child;
                final DOCUMENTTYPE documentType = docResult.getDocumentType();

                GM_TriangulatedSurface surface = null;

                if( documentType == DOCUMENTTYPE.tinWsp )
                {
                  monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.11" ) ); //$NON-NLS-1$
                  surface = getSurface( docResult );
                  if( surface != null )
                    LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, lengthSectionParameters, stationList, documentType, m_lengthSectionPage.isKmValues(), monitor );
                  else
                    return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.12" ) ); //$NON-NLS-1$
                  monitor.worked( 4 );
                }
                else if( documentType == DOCUMENTTYPE.tinVelo )
                {
                  monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.13" ) ); //$NON-NLS-1$
                  surface = getSurface( docResult );
                  if( surface != null )
                    LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, lengthSectionParameters, stationList, documentType, m_lengthSectionPage.isKmValues(), monitor );
                  else
                    return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.14" ) ); //$NON-NLS-1$
                  monitor.worked( 4 );
                }
              }
            }

            // for terrain values we have to ask the parent, because the terrain result is assigned to him
            final IResultMeta parent = stepResult.getParent();
            if( parent instanceof ICalcUnitResultMeta )
            {
              final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) parent;
              final IFeatureWrapperCollection<IResultMeta> calcUniChildren = calcUnitResult.getChildren();
              for( final IResultMeta child : calcUniChildren )
              {
                if( child instanceof IDocumentResultMeta )
                {
                  final IDocumentResultMeta docResult = (IDocumentResultMeta) child;
                  final DOCUMENTTYPE documentType = docResult.getDocumentType();

                  GM_TriangulatedSurface surface = null;

                  if( documentType == DOCUMENTTYPE.tinTerrain )
                  {
                    monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.15" ) ); //$NON-NLS-1$
                    surface = getSurface( docResult );
                    if( surface != null )
                    {
                      LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, lengthSectionParameters, stationList, documentType, m_lengthSectionPage.isKmValues(), monitor );
                    }
                    else
                      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.16" ) ); //$NON-NLS-1$
                  }
                }
              }
            }

            monitor.worked( 4 );

            /* write the observation gml */
            if( lsObs.getResult().size() > 0 )
            {
              monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.17" ) ); //$NON-NLS-1$
              ObservationFeatureFactory.toFeature( lsObs, lsObsWorkspace.getRootFeature() );

              final IPath docPath = resultMeta.getFullPath();
              final IFolder folder = m_scenarioFolder.getFolder( docPath );

              final IFile lsFile = folder.getFile( "lengthSection.gml" ); //$NON-NLS-1$

              /* delete the existing length section file */
              if( lsFile.exists() )
                lsFile.delete( true, true, new NullProgressMonitor() );

              final File lsObsFile = lsFile.getLocation().toFile();

              GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" ); //$NON-NLS-1$

              // if there is already a length section document, delete it
              for( final IResultMeta child : children )
              {
                if( child instanceof IDocumentResultMeta )
                {
                  final IDocumentResultMeta docResult = (IDocumentResultMeta) child;
                  if( docResult.getDocumentType() == DOCUMENTTYPE.lengthSection )
                  {
                    stepResult.removeChild( docResult );
                    break;
                  }
                }
              }
              final BigDecimal min = new BigDecimal( 0 );
              final BigDecimal max = new BigDecimal( 0 );
              ResultMeta1d2dHelper.addDocument( stepResult, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.20" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.21" ), IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( "lengthSection.gml" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            }
            else
            {
              return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.23" ) ); //$NON-NLS-1$
            }
            monitor.worked( 1 );
          }
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.24" ) ); //$NON-NLS-1$
        }
        catch( final Throwable t )
        {
          throw new InvocationTargetException( t );
        }
        finally
        {
          monitor.done();
        }
        return StatusUtilities.createOkStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.25" ) ); //$NON-NLS-1$

      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.26" ), status ); //$NON-NLS-1$

    if( status.matches( IStatus.ERROR ) || status.matches( IStatus.WARNING ) )
      return false;
    else
      return true;

  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;

  }

  private GM_TriangulatedSurface getSurface( final IDocumentResultMeta docResult )
  {
    final GM_TriangulatedSurface surface = null;

    final IPath docPath = docResult.getFullPath();
    if( docPath == null )
      return null;

    try
    {
      final URL scenarioURL = ResourceUtilities.createURL( m_scenarioFolder );
      final URL surfaceURL = UrlUtilities.resolveWithZip( scenarioURL, docPath.toPortableString() );

      final GMLWorkspace w = GmlSerializer.createGMLWorkspace( surfaceURL, null );

      final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      final GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryPropertyValue();

      if( geometryProperty instanceof GM_TriangulatedSurface )
        return (GM_TriangulatedSurface) geometryProperty;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return surface;
  }
}
