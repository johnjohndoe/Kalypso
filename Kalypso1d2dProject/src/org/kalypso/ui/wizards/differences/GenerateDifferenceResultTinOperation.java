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
package org.kalypso.ui.wizards.differences;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;

import javax.activation.UnsupportedDataTypeException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.DifferenceResultTinHandler;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.MathOperator;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.MinMaxCatcher;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

public final class GenerateDifferenceResultTinOperation implements ICoreRunnableWithProgress
{
  private final MinMaxCatcher m_minMaxCatcher = new MinMaxCatcher();

  private final IFolder m_scenarioFolder;

  private final MathOperator m_operator;

  private final IResultMeta[] m_masterResults;

  private final IResultMeta[] m_destinationResults;

  private final IResultMeta[] m_slaveResults;

  public GenerateDifferenceResultTinOperation( final MathOperator operator, final IResultMeta[] masterResults, final IResultMeta[] destinationResults, final IResultMeta[] slaveResults, final IFolder scenarioFolder )
  {
    m_operator = operator;
    m_masterResults = masterResults;
    m_destinationResults = destinationResults;
    m_slaveResults = slaveResults;
    m_scenarioFolder = scenarioFolder;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    monitor.beginTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.17"), 100 ); //$NON-NLS-1$

    try
    {
      GM_TriangulatedSurface masterSurface = null;
      GM_TriangulatedSurface slaveSurface = null;

      monitor.subTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.18") ); //$NON-NLS-1$
      masterSurface = getSurfaceData( m_masterResults[0] );
      monitor.worked( 10 );
      if( masterSurface == null )
        return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.19" ) ); //$NON-NLS-1$

      monitor.subTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.20") ); //$NON-NLS-1$
      slaveSurface = getSurfaceData( m_slaveResults[0] );
      monitor.worked( 10 );
      if( slaveSurface == null )
        return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.21" ) ); //$NON-NLS-1$

      IResultMeta destResult = null;

      // take the first selected step result FIXME: why multiple results here, makes no sense
      for( final IResultMeta resultMeta : m_destinationResults )
      {
        if( resultMeta instanceof IStepResultMeta )
        {
          destResult = resultMeta;
        }
      }

      if( destResult == null )
        return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.22" ) ); //$NON-NLS-1$

      monitor.subTask( Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.23") ); //$NON-NLS-1$

      final IPath docPath = destResult.getFullPath();
      final IFolder folder = m_scenarioFolder.getFolder( docPath );

      /* generate unique name for difference file */
      final String name = "tin"; //$NON-NLS-1$
      final String extension = ".gmlz"; //$NON-NLS-1$
      final File parentDir = docPath.toFile();

      // check, if file already exists and get the unique name */
      final File tinPath = new File( parentDir, "Tin" ); //$NON-NLS-1$
      final String uniqueFileName = FileUtilities.createNewUniqueFileName( name, extension, tinPath );

      final IFolder destPath = folder.getFolder( "Tin" ); //$NON-NLS-1$
      final IFile destFile = destPath.getFile( uniqueFileName );

      final DifferenceResultTinHandler differenceHandler = new DifferenceResultTinHandler( masterSurface, slaveSurface, m_operator, m_minMaxCatcher );
      differenceHandler.generateDifferences( destFile, monitor );

      monitor.worked( 3 );

      /* update the result db */
      /* create the path entry for the document */
      final int extensionIndex = destFile.getName().lastIndexOf( "." ); //$NON-NLS-1$
      final String substring = destFile.getName().substring( 0, extensionIndex );

      /* create filename */
      final String param = "DIFFERENCE"; //$NON-NLS-1$
      final String paramName = substring + "_" + param + extension; //$NON-NLS-1$

      /* we "know", that the results are stored in the "Tin" folder */
      final Path path = new Path( "Tin/" + paramName ); //$NON-NLS-1$

      // get min max via a minmaxCatcher during processing.
      final BigDecimal min = m_minMaxCatcher.getMinValue();
      final BigDecimal max = m_minMaxCatcher.getMaxValue();

      if( destResult instanceof IStepResultMeta )
      {
        // TODO: set a good description e.g.
        final String description = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.32" ); //$NON-NLS-1$

        final IStepResultMeta stepResult = (IStepResultMeta)destResult;
        ResultMeta1d2dHelper.addDocument( stepResult, "Differenzen", description, IDocumentResultMeta.DOCUMENTTYPE.tinDifference, path, Status.OK_STATUS, min, max ); //$NON-NLS-1$

        // FIXME: workspace not correctly dirty
      }
      else
      {
        throw new UnsupportedDataTypeException( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.34" ) ); //$NON-NLS-1$
        // TODO: cleanFiles();
      }

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
      return StatusUtilities.statusFromThrowable( e, Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.35") ); //$NON-NLS-1$
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

  private GM_TriangulatedSurface getSurfaceData( final IResultMeta resultMeta )
  {
    /* get the result data */
    if( resultMeta instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta docResult = (IDocumentResultMeta) resultMeta;

      final DOCUMENTTYPE documentType = docResult.getDocumentType();

      if( documentType == DOCUMENTTYPE.tinWsp || documentType == DOCUMENTTYPE.tinDepth || documentType == DOCUMENTTYPE.tinVelo || documentType == DOCUMENTTYPE.tinShearStress
          || documentType == DOCUMENTTYPE.tinTerrain )
      {
        try
        {
          final IPath docPath = docResult.getFullPath();
          if( docPath == null )
            return null;

          final URL scenarioURL = ResourceUtilities.createURL( m_scenarioFolder );
          final URL surfaceURL = UrlUtilities.resolveWithZip( scenarioURL, docPath.toPortableString() );

          final GMLWorkspace w = GmlSerializer.createGMLWorkspace( surfaceURL, null );

          final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

          w.accept( new TransformVisitor( targetCRS ), w.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

          final GM_Object geometryProperty = w.getRootFeature().getDefaultGeometryPropertyValue();

          if( geometryProperty instanceof GM_TriangulatedSurface )
          {
            return (GM_TriangulatedSurface) geometryProperty;
          }
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
        return null;
      }
    }
    return null;
  }
}