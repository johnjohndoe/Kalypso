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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.results.processing.ResultLSChartFile;
import org.kalypso.model.wspm.tuhh.core.results.processing.ResultLSTableFile;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.IObservationFeature;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.wspwin.core.CalculationBean;
import org.kalypso.wspwin.core.WspCfg.TYPE;
import org.kalypso.wspwin.core.prf.PrfReader;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import com.google.common.base.Charsets;

/**
 * Converts wspwin results (length section in .lng format) to wspm.
 * 
 * @author Gernot Belger
 */
public class CalculationResultConverter
{
  private final IStatusCollector m_log;

  private final TYPE m_projectType;

  private final IContainer m_wspmProject;

  private final String m_baseName;

  public CalculationResultConverter( final TYPE projectType, final String baseName, final IStatusCollector log, final IContainer wspmProject )
  {
    m_projectType = projectType;
    m_baseName = baseName;
    m_log = log;
    m_wspmProject = wspmProject;
  }

  public void convert( final CalculationBean bean, final File dathDir ) throws IOException, CoreException, GMLSchemaException, GmlSerializeException
  {
    /* Replace filename with length-section code */
    final String code = getFilenameCode();

    final String calcFilename = bean.getResultFilename( code );

    final File calcFile = new File( dathDir, calcFilename );

    if( !calcFile.isFile() )
    {
      m_log.add( IStatus.WARNING, Messages.getString( "CalculationResultConverter_0" ), null, bean.getName(), calcFilename ); //$NON-NLS-1$
      return;
    }

    /* Read the length section .lng file */
    final PrfReader prfReader = new PrfReader();
    prfReader.read( calcFile );

    /* Convert into a length section observation */
    final GMLWorkspace lengthSection = convertResults( prfReader );

    writeLengthSection( bean, lengthSection );
  }

  private GMLWorkspace convertResults( final PrfReader prfReader ) throws GMLSchemaException
  {
    final GMLWorkspace observationWorkspace = FeatureFactory.createGMLWorkspace( IObservationFeature.FEATURE_OBSERVATION, null, GmlSerializer.DEFAULT_FACTORY );

    final Feature observationFeature = observationWorkspace.getRootFeature();
    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( observationFeature );

    final Lng2LengthSection lng2LengthSection = new Lng2LengthSection( prfReader );
    lng2LengthSection.convertInto( observation );

    ObservationFeatureFactory.toFeature( observation, observationFeature );

    return observationWorkspace;
  }

  private void writeLengthSection( final CalculationBean bean, final GMLWorkspace lengthSection ) throws CoreException, IOException, GmlSerializeException
  {
    final IFolder resultFolder = m_wspmProject.getFolder( new Path( IWspmTuhhConstants.FOLDER_RESULTS ) );
    final IFolder calculationResultFolder = resultFolder.getFolder( m_baseName + bean.getName() );

    try
    {
      /* Build pathes */
      final IFolder calculationResultSubFolder = calculationResultFolder.getFolder( "Import" ); //$NON-NLS-1$

      final IPath relativeLSpath = new Path( IWspmTuhhConstants.DIR_RESULT_DATEN ).append( IWspmTuhhConstants.FILE_LAENGSSCHNITT_GML );

      final IFile lengthSectionFile = calculationResultSubFolder.getFile( relativeLSpath );

      final File dataDir = lengthSectionFile.getParent().getLocation().toFile();
      dataDir.mkdirs();

      /* GML File */
      GmlSerializer.serializeWorkspace( lengthSectionFile.getLocation().toFile(), lengthSection, Charsets.UTF_8.name() );

      /* Chart File */
      final ResultLSChartFile chartFile = new ResultLSChartFile( dataDir.getParentFile(), StringUtils.EMPTY, false, IWspmTuhhConstants.FILE_LAENGSSCHNITT_GML, bean.getName(), null, "root" ); //$NON-NLS-1$
      final IStatus chartLog = chartFile.writeFile();
      if( !chartLog.isOK() )
        m_log.add( chartLog );

      /* Table file */
      final ResultLSTableFile tableFile = new ResultLSTableFile( dataDir.getParentFile(), StringUtils.EMPTY, IWspmTuhhConstants.FILE_LAENGSSCHNITT_GML );
      final IStatus tableLog = tableFile.writeFile();
      if( !tableLog.isOK() )
        m_log.add( tableLog );
    }
    finally
    {
      calculationResultFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
  }

  private String getFilenameCode( )
  {
    switch( m_projectType )
    {
      case PASCHE:
        return "wl"; //$NON-NLS-1$

      case KNAUF:
        return "pl"; //$NON-NLS-1$
    }

    throw new IllegalArgumentException();
  }
}