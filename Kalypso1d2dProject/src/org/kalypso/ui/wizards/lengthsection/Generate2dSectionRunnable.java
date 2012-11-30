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
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandlerParameters;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * @author Thomas Jung
 *
 */
final class Generate2dSectionRunnable implements ICoreRunnableWithProgress
{
  private final IResultMeta[] m_results;

  private final IFolder m_scenarioFolder;

  private final LengthSectionHandlerParameters m_parameters;

  Generate2dSectionRunnable( final IResultMeta[] results, final LengthSectionHandlerParameters parameters, final IFolder scenarioFolder )
  {
    m_results = results;
    m_parameters = parameters;
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.7" ), 12 ); //$NON-NLS-1$

    try
    {
      monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.8" ) ); //$NON-NLS-1$

      final LineSampler sampler = new LineSampler( m_parameters );
      final BigDecimal[] stationList = sampler.calculateStations();

      // TODO: go on
      // if( stationList == null )
      //            return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.9" ) ); //$NON-NLS-1$

      monitor.worked( 3 );

      // get the Observation Template from the resources
      final URL lsObsUrl = LengthSectionHandler2d.class.getResource( "resources/lengthSectionTemplate.gml" ); //$NON-NLS-1$
      final GMLWorkspace lsObsWorkspace = GmlSerializer.createGMLWorkspace( lsObsUrl, null );
      final IObservation<TupleResult> lsObs = ObservationFeatureFactory.toObservation( lsObsWorkspace.getRootFeature() );

      // just get the first selected item
      final IResultMeta resultMeta = m_results[0];

      /* get the result data */

      if( resultMeta instanceof IStepResultMeta )
      {
        final IStepResultMeta stepResult = (IStepResultMeta) resultMeta;

        final IFeatureBindingCollection<IResultMeta> children = stepResult.getChildren();
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
                LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, m_parameters, stationList, documentType, monitor );
              else
                return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.12" ) ); //$NON-NLS-1$
              monitor.worked( 4 );
            }
            else if( documentType == DOCUMENTTYPE.tinVelo )
            {
              monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.13" ) ); //$NON-NLS-1$
              surface = getSurface( docResult );
              if( surface != null )
                LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, m_parameters, stationList, documentType, monitor );
              else
                return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.14" ) ); //$NON-NLS-1$
              monitor.worked( 4 );
            }
          }
        }

        // for terrain values we have to ask the parent, because the terrain result is assigned to him
        final IResultMeta parent = stepResult.getOwner();
        if( parent instanceof ICalcUnitResultMeta )
        {
          final ICalcUnitResultMeta calcUnitResult = (ICalcUnitResultMeta) parent;
          final IFeatureBindingCollection<IResultMeta> calcUniChildren = calcUnitResult.getChildren();
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
                  LengthSectionHandler2d.handle2DLenghtsection( lsObs, surface, m_parameters, stationList, documentType, monitor );
                }
                else
                  return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.16" ) ); //$NON-NLS-1$
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

          // allow multiple lengthsections
          // how to delete them?
          final String riverName = m_parameters.getSelectedRiverName();
          final String lengthSecionFileName = "section_" + riverName + ".gml"; //$NON-NLS-1$ //$NON-NLS-2$
          final String sectionName = riverName;

          final IFile lsFile = folder.getFile( lengthSecionFileName ); //$NON-NLS-1$

          /* delete the existing length section file */
          if( lsFile.exists() )
            lsFile.delete( true, true, new NullProgressMonitor() );

          final File lsObsFile = lsFile.getLocation().toFile();

          GmlSerializer.serializeWorkspace( lsObsFile, lsObsWorkspace, "CP1252" ); //$NON-NLS-1$

          // if there is already a length section document wirh the same name, delete it

          final IResultMeta[] childArray = children.toArray( new IResultMeta[children.size()] );
          for( final IResultMeta child : childArray )
          {
            if( child instanceof IDocumentResultMeta )
            {
              final IDocumentResultMeta docResult = (IDocumentResultMeta) child;
              if( docResult.getDocumentType() == DOCUMENTTYPE.lengthSection )
              {
                if( docResult.getName().equals( sectionName ) )
                {
                  stepResult.removeChild( docResult );
                  // break;
                }
              }
            }
          }
          final BigDecimal min = new BigDecimal( 0 );
          final BigDecimal max = new BigDecimal( 0 );
          //          ResultMeta1d2dHelper.addDocument( stepResult, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.20" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.21" ), IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( "lengthSection.gml" ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          ResultMeta1d2dHelper.addDocument( stepResult, sectionName, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.21" ), IDocumentResultMeta.DOCUMENTTYPE.lengthSection, new Path( lengthSecionFileName ), Status.OK_STATUS, min, max ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        else
        {
          return new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.23" ) ); //$NON-NLS-1$
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
    return new Status( IStatus.OK, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.25" ) ); //$NON-NLS-1$

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