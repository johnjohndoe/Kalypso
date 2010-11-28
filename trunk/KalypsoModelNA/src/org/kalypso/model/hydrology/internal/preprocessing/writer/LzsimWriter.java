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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.initialValues.Catchment;
import org.kalypso.model.hydrology.binding.initialValues.Channel;
import org.kalypso.model.hydrology.binding.initialValues.IniHyd;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author huebsch
 */
public class LzsimWriter
{
  private final static String LZS_FORMAT_STRING = "%s h     1 %s\n   1%9.2f%9.2f\n"; //$NON-NLS-1$

  private final static String LZG_FORMAT_STRING = "%s h   1 %s\n   1%9.3f\n"; //$NON-NLS-1$

  private final InitialValues m_initialValues;

  private final HydroHash m_hydroHash;

  private final IDManager m_idManager;

  private final NAControl m_metaControl;

  private final Logger m_logger;

  public LzsimWriter( final IDManager idManager, final HydroHash hydroHash, final InitialValues initialValues, final NAControl metaControl, final Logger logger )
  {
    m_idManager = idManager;
    m_hydroHash = hydroHash;
    m_initialValues = initialValues;
    m_metaControl = metaControl;
    m_logger = logger;
  }

  public void writeLzsimFiles( final File lzsimDir ) throws NAPreprocessorException
  {
    try
    {
      doWriteLzsimFiles( lzsimDir );
    }
    catch( final Exception e )
    {
      final String msg = String.format( "Failed to write Kalypso-NA ASCII start condition: %s", e.getLocalizedMessage() );
      throw new NAPreprocessorException( msg, e );
    }
  }

  private void doWriteLzsimFiles( final File lzsimDir ) throws SimulationException
  {
    if( m_initialValues == null )
    {
      final Date simulationStart = m_metaControl.getSimulationStart();
      final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.26", simulationStart ); //$NON-NLS-1$
      m_logger.info( msg );
      return;
    }

    lzsimDir.mkdirs();

    // Initial value date
    final Date initialDate = m_initialValues.getInitialDate();
    final DateFormat dateFormat = NATimeSettings.getInstance().getLzsLzgDateFormat();
    final String iniDate = dateFormat.format( initialDate );

    writeLzg( lzsimDir, iniDate );
    writeLzs( lzsimDir, iniDate );
  }

  private void writeLzg( final File lzsimDir, final String iniDate ) throws SimulationException
  {
    final Map<String, org.kalypso.model.hydrology.binding.model.Channel> naChannelHash = buildChannelHash( m_idManager );

    // write initial conditions for the strands
    // TODO:write only for strands of the actual calculation
    final IFeatureBindingCollection<Channel> channels = m_initialValues.getChannels();
    for( final Channel iniChannel : channels )
    {
      final String naChannelID = iniChannel.getNaChannelID();
      final org.kalypso.model.hydrology.binding.model.Channel naChannel = naChannelHash.get( naChannelID );
      if( naChannel != null )
      {
        final int asciiChannelID = m_idManager.getAsciiID( naChannel );
        final String fileName = String.format( "we%s.lzg", asciiChannelID ); //$NON-NLS-1$
        final File lzgFile = new File( lzsimDir, fileName );
        writeLzgFile( iniChannel, lzgFile, iniDate );
      }
    }
  }

  private static Map<String, org.kalypso.model.hydrology.binding.model.Channel> buildChannelHash( final IDManager idManager )
  {
    final List<Feature> allNAChannelFeatures = idManager.getAllFeaturesFromType( IDManager.CHANNEL );
    final Map<String, org.kalypso.model.hydrology.binding.model.Channel> naChannelHash = new HashMap<String, org.kalypso.model.hydrology.binding.model.Channel>();
    for( final Feature feature : allNAChannelFeatures )
      naChannelHash.put( feature.getId(), (org.kalypso.model.hydrology.binding.model.Channel) feature );
    return naChannelHash;
  }

  private static void writeLzgFile( final Channel iniChannel, final File lzgFile, final String iniDate ) throws SimulationException
  {
    try
    {
      final String lzgContent = String.format( Locale.US, LZG_FORMAT_STRING, iniDate, "qgs", iniChannel.getQgs() ); //$NON-NLS-1$ 
      FileUtils.writeStringToFile( lzgFile, lzgContent );
    }
    catch( final IOException e )
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.55", iniChannel.getId() );
      throw new SimulationException( msg, e );
    }
  }

  private void writeLzs( final File lzsimDir, final String iniDate ) throws SimulationException
  {
    final Map<org.kalypso.model.hydrology.binding.model.Catchment, Catchment> iniCatchmentHash = buildCatchmentHash();

    // in the catchmentIDToFeatureHash (HashMap<featureID, feature>) are all channels in the model. if this run is
    // only a subnet of the total model the idManager only knows the featuers in the submodel
    for( final Entry<org.kalypso.model.hydrology.binding.model.Catchment, Catchment> entry : iniCatchmentHash.entrySet() )
    {
      final org.kalypso.model.hydrology.binding.model.Catchment naCatchment = entry.getKey();
      final Catchment iniCatchment = entry.getValue();

      final int asciiCatchmentID = m_idManager.getAsciiID( naCatchment );
      final String fileName = String.format( "we%s.lzs", asciiCatchmentID ); //$NON-NLS-1$
      final File lzsFile = new File( lzsimDir, fileName );

      final IniHyd[] iniHyds = getIniHyds( naCatchment, iniCatchment );
      if( iniHyds != null )
        writeLzsFile( lzsFile, iniCatchment, iniDate, iniHyds );
    }
  }

  private Map<org.kalypso.model.hydrology.binding.model.Catchment, Catchment> buildCatchmentHash( ) throws SimulationException
  {
    final List<Feature> allNACatchmentFeatures = m_idManager.getAllFeaturesFromType( IDManager.CATCHMENT );

    /* Hash ini catchments for quicker access */
    final IFeatureBindingCollection<Catchment> iniCatchments = m_initialValues.getCatchments();
    final Map<String, Catchment> iniCatchmentHash = new HashMap<String, Catchment>();
    for( final Catchment iniCatchment : iniCatchments )
    {
      final String naCatchmentID = iniCatchment.getNaCatchmentID();
      iniCatchmentHash.put( naCatchmentID, iniCatchment );
    }

    /* build the result mapping */
    final Map<org.kalypso.model.hydrology.binding.model.Catchment, Catchment> result = new HashMap<org.kalypso.model.hydrology.binding.model.Catchment, Catchment>();
    for( final Feature catchment : allNACatchmentFeatures )
    {
      final String catchmentID = catchment.getId();
      final Catchment iniCatchment = iniCatchmentHash.get( catchmentID );
      if( iniCatchment == null )
      {
        // FIXME: better? only log...
        final String msg = String.format( "Missing start condition for catchment: %s", catchmentID );
        throw new SimulationException( msg );
      }
      else
        result.put( (org.kalypso.model.hydrology.binding.model.Catchment) catchment, iniCatchment );
    }

    return result;
  }

  private static void writeLzsFile( final File lzsFile, final Catchment iniCatchment, final String iniDate, final IniHyd[] iniHyds ) throws SimulationException
  {
    PrintWriter writer = null;

    try
    {
      writer = new PrintWriter( lzsFile );

      // snow
      final Double h = iniCatchment.getH();
      final Double ws = iniCatchment.getWS();
      writer.format( Locale.US, LZS_FORMAT_STRING, iniDate, "snow", h, ws ); //$NON-NLS-1$ 

      // groundwater
      final Double hgws = iniCatchment.getHwgs();
      final Double qb = iniCatchment.getQb();
      writer.format( Locale.US, LZS_FORMAT_STRING, iniDate, "gwsp", hgws, qb ); //$NON-NLS-1$

      // hydrotops (interception storage content& soil moisture)
      writer.format( Locale.US, "%s h  %4d bodf\n", iniDate, iniHyds.length ); //$NON-NLS-1$ 

      for( int i = 0; i < iniHyds.length; i++ )
      {
        final IniHyd iniHyd = iniHyds[i];
        final Double bi = iniHyd.getBi();

        writer.format( Locale.US, "%4d%7.2f", i + 1, bi ); //$NON-NLS-1$

        final List<Double> bofs = iniHyd.getBofs();
        for( final Double bof : bofs )
          writer.format( Locale.US, "%7.2f", bof ); //$NON-NLS-1$

        writer.append( "\n" );
      }
      writer.close();
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Failed to write initial condition for channel %s", iniCatchment.getNaCatchmentID() );
      throw new SimulationException( msg, e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private IniHyd[] getIniHyds( final org.kalypso.model.hydrology.binding.model.Catchment naCatchment, final Catchment iniCatchment )
  {
    /*
     * Special case: if the hydro hash does not know this catchment, it was actually never written. This happens e.g.
     * for catchments that are targets of groundwater flow, but which are not part of the currently written sub-net. We
     * ignore this case silently for now.
     */
    final List<HydrotopeInfo> hydrotops = m_hydroHash.getHydrotops( naCatchment );
    if( hydrotops.size() == 0 )
      return null;

    /* Hash ini hydrotopes for quicker access */
    final IFeatureBindingCollection<IniHyd> iniHyds = iniCatchment.getIniHyds();
    final Map<String, IniHyd> iniHydHash = new HashMap<String, IniHyd>();
    for( final IniHyd iniHyd : iniHyds )
    {
      final String naHydrotopID = iniHyd.getNaHydrotopID();
      iniHydHash.put( naHydrotopID, iniHyd );
    }

    /* Build result map */
    final Map<Integer, IniHyd> iniHydMap = new TreeMap<Integer, IniHyd>();
    for( final HydrotopeInfo hydrotopeInfo : hydrotops )
    {
      final IHydrotope hydrotop = hydrotopeInfo.getHydrotop();
      final Integer localID = hydrotopeInfo.getLocalID();
      final String naHydrotopID = hydrotop.getId();

      final IniHyd iniHyd = iniHydHash.get( naHydrotopID );

      iniHydMap.put( localID, iniHyd );
    }

    return iniHydMap.values().toArray( new IniHyd[iniHydMap.size()] );
  }

}