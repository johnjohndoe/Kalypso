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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.model.hydrology.binding.initialValues.Catchment;
import org.kalypso.model.hydrology.binding.initialValues.Channel;
import org.kalypso.model.hydrology.binding.initialValues.IniHyd;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author huebsch
 */
class LzsimWriter
{
  private static final String LZS_FORMAT_STRING = "%s h     1 %s\n   1%9.2f%9.2f\n"; //$NON-NLS-1$

  private static final String LZG_FORMAT_STRING = "%s h   1 %s\n   1%9.3f\n"; //$NON-NLS-1$

  private final InitialValues m_initialValues;

  private final IDManager m_idManager;

  private final NaCatchmentData m_catchmentData;

  public LzsimWriter( final IDManager idManager, final NaCatchmentData catchmentData, final InitialValues initialValues )
  {
    m_idManager = idManager;
    m_catchmentData = catchmentData;
    m_initialValues = initialValues;
  }

  public void writeLzsimFiles( final File lzsimDir ) throws NAPreprocessorException
  {
    doWriteLzsimFiles( lzsimDir );
  }

  private void doWriteLzsimFiles( final File lzsimDir ) throws NAPreprocessorException
  {
    lzsimDir.mkdirs();

    if( m_initialValues == null )
      return;

    // Initial value date
    final Date initialDate = m_initialValues.getInitialDate();
    final DateFormat dateFormat = NATimeSettings.getInstance().getLzsLzgDateFormat();
    final String iniDate = dateFormat.format( initialDate );

    writeLzg( lzsimDir, iniDate );
    writeLzs( lzsimDir, iniDate );
  }

  private void writeLzg( final File lzsimDir, final String iniDate ) throws NAPreprocessorException
  {
    final Map<String, org.kalypso.model.hydrology.binding.model.channels.Channel> naChannelHash = buildChannelHash( m_idManager );

    // write initial conditions for the strands
    // TODO: write only for strands of the actual calculation

    final IFeatureBindingCollection<Channel> channels = m_initialValues.getChannels();
    for( final Channel iniChannel : channels )
    {
      final String naChannelID = iniChannel.getNaChannelID();
      final org.kalypso.model.hydrology.binding.model.channels.Channel naChannel = naChannelHash.get( naChannelID );

//      if( naChannel instanceof VirtualChannel )
//        continue;

      if( naChannel == null )
      {
        // FIXME: we can only check this, if we iterate over the relevant model-channels
        // throw new NAPreprocessorException( "Missing " );
        continue;
      }

      final int asciiChannelID = m_idManager.getAsciiID( naChannel );
      final String fileName = String.format( "we%s.lzg", asciiChannelID ); //$NON-NLS-1$
      final File lzgFile = new File( lzsimDir, fileName );

      final Double qgs = iniChannel.getQgs();
      if( qgs == null )
      {
        final String msg = String.format( "Channel '%s': missing start condition value (qgs)", naChannel.getName() ); //$NON-NLS-1$
        throw new NAPreprocessorException( msg );
      }

      writeLzgFile( iniChannel, lzgFile, iniDate, qgs );
    }
  }

  private static Map<String, org.kalypso.model.hydrology.binding.model.channels.Channel> buildChannelHash( final IDManager idManager )
  {
    final List<Feature> allNAChannelFeatures = idManager.getAllFeaturesFromType( IDManager.CHANNEL );
    final Map<String, org.kalypso.model.hydrology.binding.model.channels.Channel> naChannelHash = new HashMap<>();
    for( final Feature feature : allNAChannelFeatures )
      naChannelHash.put( feature.getId(), (org.kalypso.model.hydrology.binding.model.channels.Channel)feature );
    return naChannelHash;
  }

  private static void writeLzgFile( final Channel iniChannel, final File lzgFile, final String iniDate, final Double qgs ) throws NAPreprocessorException
  {
    try
    {
      final String lzgContent = String.format( Locale.US, LZG_FORMAT_STRING, iniDate, "qgs", qgs ); //$NON-NLS-1$
      FileUtils.writeStringToFile( lzgFile, lzgContent );
    }
    catch( final IOException e )
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.55", iniChannel.getId() ); //$NON-NLS-1$
      throw new NAPreprocessorException( msg, e );
    }
  }

  private void writeLzs( final File lzsimDir, final String iniDate ) throws NAPreprocessorException
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

  private Map<org.kalypso.model.hydrology.binding.model.Catchment, Catchment> buildCatchmentHash( ) throws NAPreprocessorException
  {// FIXME: use catchmentData instead!
    final List<Feature> allNACatchmentFeatures = m_idManager.getAllFeaturesFromType( IDManager.CATCHMENT );

    /* Hash ini catchments for quicker access */
    final IFeatureBindingCollection<Catchment> iniCatchments = m_initialValues.getCatchments();
    final Map<String, Catchment> iniCatchmentHash = new HashMap<>();
    for( final Catchment iniCatchment : iniCatchments )
    {
      final String naCatchmentID = iniCatchment.getNaCatchmentID();
      iniCatchmentHash.put( naCatchmentID, iniCatchment );
    }

    /* build the result mapping */
    final Map<org.kalypso.model.hydrology.binding.model.Catchment, Catchment> result = new HashMap<>();
    for( final Feature catchment : allNACatchmentFeatures )
    {
      final String catchmentID = catchment.getId();
      final Catchment iniCatchment = iniCatchmentHash.get( catchmentID );
      if( iniCatchment == null )
      {
        // FIXME: better? only log...
        final String msg = Messages.getString( "LzsimWriter.5", catchment.getName() ); //$NON-NLS-1$
        throw new NAPreprocessorException( msg );
      }
      else
        result.put( (org.kalypso.model.hydrology.binding.model.Catchment)catchment, iniCatchment );
    }

    return result;
  }

  private static void writeLzsFile( final File lzsFile, final Catchment iniCatchment, final String iniDate, final IniHyd[] iniHyds ) throws NAPreprocessorException
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

        writer.append( "\n" ); //$NON-NLS-1$
      }
      writer.close();
    }
    catch( final Throwable e )
    {
      final String msg = String.format( Messages.getString( "LzsimWriter.7" ), iniCatchment.getNaCatchmentID() ); //$NON-NLS-1$
      throw new NAPreprocessorException( msg, e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private IniHyd[] getIniHyds( final org.kalypso.model.hydrology.binding.model.Catchment naCatchment, final Catchment iniCatchment ) throws NAPreprocessorException
  {
    /*
     * Special case: if the hydro hash does not know this catchment, it was actually never written. This happens e.g.
     * for catchments that are targets of groundwater flow, but which are not part of the currently written sub-net. We
     * ignore this case silently for now.
     */
    final List<HydrotopeInfo> hydrotops = m_catchmentData.getHydrotops( naCatchment );
    if( hydrotops.size() == 0 )
      return null;

    /* Hash ini hydrotopes for quicker access */
    final IFeatureBindingCollection<IniHyd> iniHyds = iniCatchment.getIniHyds();
    final Map<String, IniHyd> iniHydHash = new HashMap<>();
    for( final IniHyd iniHyd : iniHyds )
    {
      final String naHydrotopID = iniHyd.getNaHydrotopID();
      iniHydHash.put( naHydrotopID, iniHyd );
    }

    /* Build result map */
    final List<IniHyd> result = new ArrayList<>( hydrotops.size() );

    for( final HydrotopeInfo hydrotopeInfo : hydrotops )
    {
      final String naHydrotopID = hydrotopeInfo.getFeatureId();

      final IniHyd iniHyd = iniHydHash.get( naHydrotopID );
      if( iniHyd == null )
      {
        final String msg = Messages.getString( "LzsimWriter.4", naHydrotopID, naCatchment.getName() ); //$NON-NLS-1$
        throw new NAPreprocessorException( msg );
      }

      result.add( iniHyd );
    }

    return result.toArray( new IniHyd[result.size()] );
  }
}