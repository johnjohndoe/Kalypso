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
package org.kalypso.ui.rrm.internal.scenarios;

import javax.xml.namespace.QName;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.gml.compare.FeatureListComparator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

import de.renew.workflow.connector.cases.IScenario;

/**
 * This operation compares the selected scenarios with one other scenario.
 * 
 * @author Holger Albert
 */
public class CompareScenariosOperation implements ICoreRunnableWithProgress
{
  /**
   * The scenarios data object.
   */
  private final MergeScenariosData m_scenariosData;

  /**
   * The scenario compare status contains stati for several cases.
   */
  private final ScenarioCompareStatus m_compareStatus;

  /**
   * The constructor.
   * 
   * @param scenariosData
   *          The scenarios data object.
   * @param compareStatus
   *          The scenario compare status contains stati for several cases.
   */
  public CompareScenariosOperation( final MergeScenariosData scenariosData, final ScenarioCompareStatus compareStatus )
  {
    m_scenariosData = scenariosData;
    m_compareStatus = compareStatus;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    /* Get the target scenario. */
    final IScenario targetScenario = m_scenariosData.getTargetScenario();

    try
    {
      /* Get the selected scenarios. */
      final IScenario[] selectedScenarios = m_scenariosData.getSelectedScenarios();
      if( selectedScenarios == null || selectedScenarios.length == 0 )
        throw new IllegalArgumentException( Messages.getString( "CompareScenariosOperation_0" ) ); //$NON-NLS-1$

      /* Monitor. */
      monitor.beginTask( String.format( Messages.getString( "CompareScenariosOperation_1" ), targetScenario.getName() ), 750 * selectedScenarios.length ); //$NON-NLS-1$

      /* Get the reference rrm scenario. */
      final IFolder referenceScenariofolder = targetScenario.getFolder();
      final RrmScenario referenceRrmScenario = new RrmScenario( referenceScenariofolder );

      /* Get the files of the reference rrm scenario. */
      final IFile referenceModelFile = referenceRrmScenario.getModelFile();
      final IFile referenceParameterGml = referenceRrmScenario.getParameterGml();
      final IFile referenceHydrotopGml = referenceRrmScenario.getHydrotopGml();

      /* Loop all selected scenarios. */
      for( final IScenario selectedScenario : selectedScenarios )
      {
        /* Get the selected rrm scenario. */
        final IFolder selectedFolder = selectedScenario.getFolder();
        final RrmScenario selectedRrmScenario = new RrmScenario( selectedFolder );

        /* Get the files of the selected rrm scenario. */
        final IFile selectedModelFile = selectedRrmScenario.getModelFile();
        final IFile selectedParameterGml = selectedRrmScenario.getParameterGml();
        final IFile selectedHydrotopGml = selectedRrmScenario.getHydrotopGml();

        /* Monitor. */
        monitor.subTask( Messages.getString( "CompareScenariosOperation_2" ) ); //$NON-NLS-1$

        /* Compare. */
        compare( referenceModelFile, new CompareData( selectedScenario, ScenarioCompareStatus.KEY_MODEL, selectedModelFile, new GMLXPath[] {
            new GMLXPath( NaModell.MEMBER_CATCHMENT_COLLECTION ).append( new GMLXPath( NaModell.MEMBER_CATCHMENT ) ),
            new GMLXPath( NaModell.MEMBER_CHANNEL_COLLECTION ).append( new GMLXPath( NaModell.MEMBER_CHANNEL ) ),
            new GMLXPath( NaModell.MEMBER_NODE_COLLECTION ).append( new GMLXPath( NaModell.MEMBER_NODE ) ) }, new QName[] { NaModell.QN_NAME, FeatureListComparator.PROPERTY_COUNTER, NaModell.QN_NAME } ) );

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( Messages.getString( "CompareScenariosOperation_3" ) ); //$NON-NLS-1$

        /* Compare. */
        compare( referenceParameterGml, new CompareData( selectedScenario, ScenarioCompareStatus.KEY_PARAMETER, selectedParameterGml, new GMLXPath[] { new GMLXPath( Parameter.MEMBER_SNOW ),
            new GMLXPath( Parameter.MEMBER_SOILTYPE ), new GMLXPath( Parameter.MEMBER_DRWBM_SOILTYPE ) }, new QName[] { Parameter.QN_NAME, Parameter.QN_NAME, Parameter.QN_NAME } ) );

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( Messages.getString( "CompareScenariosOperation_4" ) ); //$NON-NLS-1$

        /* Compare. */
        compare( referenceHydrotopGml, new CompareData( selectedScenario, ScenarioCompareStatus.KEY_HYDROTOPES, selectedHydrotopGml, new GMLXPath[] { new GMLXPath( HydrotopeCollection.MEMBER_HYDROTOPE ) }, new QName[] { FeatureListComparator.PROPERTY_COUNTER } ) );

        /* Monitor. */
        monitor.worked( 250 );
      }

      return collector.asMultiStatus( String.format( Messages.getString( "CompareScenariosOperation_5" ), targetScenario.getName() ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( Messages.getString( "CompareScenariosOperation_6" ), targetScenario.getName() ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void compare( final IFile referenceFile, final CompareData compareData ) throws Exception
  {
    final String uri = compareData.getScenario().getURI();
    final String key = compareData.getKey();

    if( !m_compareStatus.hasStatus( uri, key ) )
    {
      final IStatus status = compareData( referenceFile, compareData );
      m_compareStatus.putStatus( uri, key, status );
    }
  }

  private IStatus compareData( final IFile referenceFile, final CompareData compareData ) throws Exception
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    /* Compare the file size. */
    final IStatus fileSizeStatus = compareFileSize( referenceFile, compareData );
    collector.add( fileSizeStatus );

    /* Compare the model. */
    final IStatus modelStatus = compareModel( referenceFile, compareData );
    collector.add( modelStatus );

    /* Compare something else? */
    // TODO

    /* Return with 'Changed' message. */
    if( !collector.isOK() )
      return collector.asMultiStatus( Messages.getString( "CompareScenariosOperation_7" ) ); //$NON-NLS-1$

    /* Return with 'Not changed' message. */
    return collector.asMultiStatus( Messages.getString( "CompareScenariosOperation_8" ) ); //$NON-NLS-1$

  }

  private IStatus compareFileSize( final IFile referenceFile, final CompareData compareData ) throws CoreException
  {
    final IFileStore referenceStore = EFS.getStore( referenceFile.getLocationURI() );
    final IFileInfo referenceFileInfo = referenceStore.fetchInfo();
    final long referenceLength = referenceFileInfo.getLength();

    final IFile selectedFile = compareData.getFile();
    final IFileStore selectedStore = EFS.getStore( selectedFile.getLocationURI() );
    final IFileInfo selectedFileInfo = selectedStore.fetchInfo();
    final long selectedLength = selectedFileInfo.getLength();

    if( referenceLength != selectedLength )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString("CompareScenariosOperation.0") ); //$NON-NLS-1$

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("CompareScenariosOperation.1") ); //$NON-NLS-1$
  }

  private IStatus compareModel( final IFile referenceFile, final CompareData compareData ) throws Exception
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    /* Get some data. */
    final IFile selectedFile = compareData.getFile();
    final GMLXPath[] listPaths = compareData.getListPaths();
    final QName[] uniqueProperties = compareData.getUniqueProperties();

    /* Load the workspaces. */
    final GMLWorkspace referenceWorkspace = GmlSerializer.createGMLWorkspace( referenceFile );
    final GMLWorkspace selectedWorkspace = GmlSerializer.createGMLWorkspace( selectedFile );

    /* Get the models. */
    final Feature referenceModel = referenceWorkspace.getRootFeature();
    final Feature selectedModel = selectedWorkspace.getRootFeature();

    /* Compare each configured list. */
    for( int i = 0; i < listPaths.length; i++ )
    {
      final GMLXPath listPath = listPaths[i];
      final QName uniqueProperty = uniqueProperties[i];

      final IStatus listStatus = compareList( referenceModel, selectedModel, listPath, uniqueProperty );
      collector.add( listStatus );
    }

    /* Dispose the workspaces. */
    referenceWorkspace.dispose();
    selectedWorkspace.dispose();

    /* Return with 'Changed' message. */
    if( !collector.isOK() )
      return collector.asMultiStatus( Messages.getString("CompareScenariosOperation.2") ); //$NON-NLS-1$

    /* Return with 'Not changed' message. */
    return collector.asMultiStatus( Messages.getString("CompareScenariosOperation.3") ); //$NON-NLS-1$
  }

  private IStatus compareList( final Feature referenceFeature, final Feature selectedFeature, final GMLXPath listPath, final QName uniqueProperty ) throws Exception
  {
    final FeatureListComparator comparator = new FeatureListComparator( referenceFeature, selectedFeature, listPath, uniqueProperty );
    return comparator.compareList();
  }
}