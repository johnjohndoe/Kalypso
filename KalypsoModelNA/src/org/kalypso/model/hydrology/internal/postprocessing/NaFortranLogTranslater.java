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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Translates the id's of the error.gml (written by KalypsoNA.exe) into id's of the KalypsoHydrology model.
 * 
 * @author Gernot Belger
 */
public class NaFortranLogTranslater
{
  private static final QName QNAME_ERRLOG_RECORD = new QName( NaModelConstants.NS_NAFORTRANLOG, "record" );

  private static final QName QNAME_ERRLOG_ELEMENT = new QName( NaModelConstants.NS_NAFORTRANLOG, "element" );

  private final File m_logFile;

  private final IDManager m_idManager;

  private GMLWorkspace m_workspace;

  private final Logger m_logger;

  public NaFortranLogTranslater( final File asciiDir, final IDManager idManager, final Logger logger )
  {
    m_logger = logger;
    m_logFile = new File( asciiDir, "start/error.gml" ); //$NON-NLS-1$

    m_idManager = idManager;
  }

  public void translate( final File resultFile )
  {
    readLog();
    translateLog();
    writeLog( resultFile );
  }

  private void readLog( )
  {
    try
    {
      m_workspace = GmlSerializer.createGMLWorkspace( m_logFile, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to read KalypsoNA log file %s", e.getLocalizedMessage() );
      m_logger.warning( msg );
    }
  }

  private void translateLog( )
  {
    if( m_workspace == null )
      return;

    final IFeatureType recordFT = m_workspace.getGMLSchema().getFeatureType( QNAME_ERRLOG_RECORD ); //$NON-NLS-1$
    final Feature[] recordFEs = m_workspace.getFeatures( recordFT );

    for( final Feature feature : recordFEs )
    {
      final String elementString = (String) feature.getProperty( QNAME_ERRLOG_ELEMENT ); //$NON-NLS-1$
      final int i = elementString.indexOf( "       " ); //$NON-NLS-1$
      final String fortranID = elementString.substring( i ).trim();
      if( !StringUtils.isBlank( fortranID ) )
      {
        final Integer asciiID = NumberUtils.parseQuietInteger( fortranID );
        final int asciiIDBoxed = asciiID == null ? 0 : asciiID;

        final String asciiType = findAsciiType( elementString );
        final int type = findType( elementString );

        final Feature feature2 = m_idManager.getFeature( asciiIDBoxed, type );
        if( feature2 == null )
        {
          final String element = elementString.substring( 0, i );
          final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.242", element, fortranID ); //$NON-NLS-1$ //$NON-NLS-2$
          m_logger.warning( msg );
        }
        else
        {
          final String gmlName = feature2.getName();
          feature.setName( asciiType + " " + gmlName.trim() ); //$NON-NLS-1$
          feature.setDescription( "FeatureID: " + gmlName.trim() ); //$NON-NLS-1$
        }
      }
    }
  }

  private static String findAsciiType( final String elementString )
  {
    if( elementString.contains( "Teilgebiet" ) ) //$NON-NLS-1$
      return "Teilgebiet"; //$NON-NLS-1$

    if( elementString.contains( "Knoten" ) ) //$NON-NLS-1$
      return "Knoten"; //$NON-NLS-1$

    if( elementString.contains( "Strang" ) ) //$NON-NLS-1$
      return "Strang"; //$NON-NLS-1$

    return null;
  }

  private static int findType( final String elementString )
  {
    if( elementString.contains( "Teilgebiet" ) ) //$NON-NLS-1$
      return IDManager.CATCHMENT;

    if( elementString.contains( "Knoten" ) ) //$NON-NLS-1$
      return IDManager.NODE;

    if( elementString.contains( "Strang" ) ) //$NON-NLS-1$
      return IDManager.CHANNEL;

    return 0;
  }

  private void writeLog( final File resultFile )
  {
    try
    {
      if( m_workspace == null )
      {
        final String msg = String.format( "Corrupt log-file is copied back to Kalypso. Element IDs are NOT translated." );
        m_logger.warning( msg );
        FileUtils.copyFile( m_logFile, resultFile );

        return;
      }

      GmlSerializer.serializeWorkspace( resultFile, m_workspace, "UTF-8" ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to write translated KalypsoNA log file %s", e.getLocalizedMessage() );
      m_logger.severe( msg );
    }
  }
}
