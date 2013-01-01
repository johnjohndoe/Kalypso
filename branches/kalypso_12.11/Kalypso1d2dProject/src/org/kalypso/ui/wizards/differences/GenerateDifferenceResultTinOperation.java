/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.MathOperator;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.MinMaxCatcher;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

public final class GenerateDifferenceResultTinOperation implements ICoreRunnableWithProgress
{
  private final TinDifferenceData m_data;

  public GenerateDifferenceResultTinOperation( final TinDifferenceData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.17" ), 100 ); //$NON-NLS-1$

    try
    {
      monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.18" ) ); //$NON-NLS-1$
      final GM_TriangulatedSurface masterSurface = getSurfaceData( m_data.getMasterResult() );
      monitor.worked( 10 );
      if( masterSurface == null )
        return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.19" ) ); //$NON-NLS-1$

      monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.20" ) ); //$NON-NLS-1$
      final GM_TriangulatedSurface slaveSurface = getSurfaceData( m_data.getSlaveResult() );
      monitor.worked( 10 );
      if( slaveSurface == null )
        return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.21" ) ); //$NON-NLS-1$

      monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.23" ) ); //$NON-NLS-1$

      final IStepResultMeta destResult = m_data.getDestinationResult();
      final IFolder scenarioFolder = m_data.getScenarioFolder();
      final IPath stepPath = destResult.getFullPath();
      final IFolder stepFolder = scenarioFolder.getFolder( stepPath );

      final IFile destFile = createDestinationFilename( stepFolder );

      final MathOperator operator = m_data.getOperator();
      final DifferenceResultTinHandler differenceHandler = new DifferenceResultTinHandler( masterSurface, slaveSurface, operator );
      differenceHandler.generateDifferences( destFile, monitor );

      monitor.worked( 3 );

      /* update the result db */

      /* create the path entry for the document */
      final IPath destPath = destFile.getFullPath().makeRelativeTo( stepFolder.getFullPath() );

      // get min max via a minmaxCatcher during processing.
      final MinMaxCatcher minMax = differenceHandler.getMinMax();
      final BigDecimal min = minMax.getMinValue();
      final BigDecimal max = minMax.getMaxValue();

      // TODO: set a good description e.g.
      final String description = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.32" ); //$NON-NLS-1$

      final IStepResultMeta stepResult = m_data.getDestinationResult();
      ResultMeta1d2dHelper.addDocument( stepResult, "Differenzen", description, IDocumentResultMeta.DOCUMENTTYPE.tinDifference, destPath, Status.OK_STATUS, min, max ); //$NON-NLS-1$

      // FIXME: workspace not correctly dirty

      monitor.worked( 1 );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    // FIXME: awful error handling!
    catch( final Exception e )
    {
      e.printStackTrace();
      // TODO: cleanFiles();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.35" ) ); //$NON-NLS-1$
    }
    catch( final Throwable t )
    {
      // TODO: cleanFiles();
      throw new InvocationTargetException( t );
    }
    finally
    {
      monitor.done();
    }
    return new Status( IStatus.OK, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.36" ) ); //$NON-NLS-1$
  }

  private IFile createDestinationFilename( final IFolder stepFolder )
  {
    final IFolder tinFolder = stepFolder.getFolder( "Tin" ); //$NON-NLS-1$

    /* generate unique name for difference file */
    final String prefix = "tin"; //$NON-NLS-1$
    final String suffix = String.format( "_%s.gmlz", ResultType.DIFFERENCE.name() );

    // check, if file already exists and get the unique name */
    final File tinDir = tinFolder.getLocation().toFile();
    final String uniqueFileName = FileUtilities.createNewUniqueFileName( prefix, suffix, tinDir );

    return tinFolder.getFile( uniqueFileName );
  }

  private GM_TriangulatedSurface getSurfaceData( final IDocumentResultMeta docResult )
  {
    /* get the result data */
    final DOCUMENTTYPE documentType = docResult.getDocumentType();

    if( documentType == DOCUMENTTYPE.tinWsp || documentType == DOCUMENTTYPE.tinDepth || documentType == DOCUMENTTYPE.tinVelo || documentType == DOCUMENTTYPE.tinShearStress
        || documentType == DOCUMENTTYPE.tinTerrain )
    {
      try
      {
        final IPath docPath = docResult.getFullPath();
        if( docPath == null )
          return null;

        final IFolder scenarioFolder = m_data.getScenarioFolder();
        final URL scenarioURL = ResourceUtilities.createURL( scenarioFolder );
        final URL surfaceURL = UrlUtilities.resolveWithZip( scenarioURL, docPath.toPortableString() );

        final GMLWorkspace w = GmlSerializer.createGMLWorkspace( surfaceURL, null );

        final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

        w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

        final GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryPropertyValue();

        if( geometryProperty instanceof GM_TriangulatedSurface )
        {
          return (GM_TriangulatedSurface)geometryProperty;
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      return null;
    }
    return null;
  }
}