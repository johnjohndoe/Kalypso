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
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.text.DateFormat;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author huebsch
 */
public class LzsimManager
{
  private final Date[] m_initialDates;

  private final File m_outputDir;

  public LzsimManager( final Date[] initialDates, final File outputDir )
  {
    m_initialDates = initialDates;
    m_outputDir = outputDir;
  }

  /**
   * Reads the initial values back from the ascii files, if any have been ordered.
   */
  public void readInitialValues( final IDManager idManager, final HydroHash hydroHash, final File lzsimDir, final Logger logger ) throws Exception
  {
    if( m_initialDates.length == 0 )
      return;

    for( final Date initialDate : m_initialDates )
    {
      final LzsToGml lzsToGml = new LzsToGml( lzsimDir, initialDate, idManager, hydroHash, logger );
      lzsToGml.readLzs();
      lzsToGml.writeGml( m_outputDir );
    }
  }

  public static void writeLzsimFiles( final IDManager idManager, final HydroHash hydroHash, final File lzsimDir, final GMLWorkspace iniValuesWorkspace )
  {
    final List<Feature> allNAChannelFeatures = idManager.getAllFeaturesFromType( IDManager.CHANNEL );
    final Hashtable<String, Feature> channelIDToFeatureHash = new Hashtable<String, Feature>();
    for( final Feature feature : allNAChannelFeatures )
      channelIDToFeatureHash.put( feature.getId(), feature );

    final List<Feature> allNACatchmentFeatures = idManager.getAllFeaturesFromType( IDManager.CATCHMENT );
    final Hashtable<String, Feature> catchmentIDToFeatureHash = new Hashtable<String, Feature>();
    for( final Feature feature : allNACatchmentFeatures )
      catchmentIDToFeatureHash.put( feature.getId(), feature );

    final Feature iniValuesRootFeature = iniValuesWorkspace.getRootFeature();
    // Initial value date
    final Date initialDate = DateUtilities.toDate( (XMLGregorianCalendar) iniValuesRootFeature.getProperty( new QName( NaModelConstants.NS_INIVALUES, "iniDate" ) ) ); //$NON-NLS-1$
    final DateFormat dateFormat = NATimeSettings.getInstance().getLzsLzgDateFormat();
    final String iniDate = dateFormat.format( initialDate );

    // write initial conditions for the strands
    // TODO:write only for strands of the actual calculation
    final List< ? > channelList = (List< ? >) iniValuesRootFeature.getProperty( NaModelConstants.INI_CHANNEL_MEMBER_PROP );
    for( int i = 0; i < channelList.size(); i++ )
    {
      final Feature channelFE = (Feature) channelList.get( i );
      final StringBuffer lzgBuffer = new StringBuffer();
      final String naChannelID = (String) channelFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ) ); //$NON-NLS-1$

      final Feature naChannelFE = channelIDToFeatureHash.get( naChannelID );
      // in the channelIDToFeatureHash (HashMap<featureID, feature>) are all channels in the model. if this run is only
      // a subnet of the total model the idManager only knows the featuers in the submodel just skip this one.
      if( naChannelFE == null )
        continue;
      final int asciiChannelID = idManager.getAsciiID( naChannelFE );
      final String fileName = "we" + asciiChannelID + ".lzg"; //$NON-NLS-1$//$NON-NLS-2$

      try
      {
        final File lzgFile = new File( lzsimDir, fileName );
        lzgBuffer.append( iniDate + " h   1 qgs" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        final Double h = (Double) channelFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "qgs" ) ); //$NON-NLS-1$
        lzgBuffer.append( "   1" + FortranFormatHelper.printf( h, "f9.3" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        final Writer lzgWriter = new FileWriter( lzgFile );
        lzgWriter.write( lzgBuffer.toString() );
        lzgWriter.close();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.55", channelFE.getId() ) ); //$NON-NLS-1$
      }
    }

    // for all catchments in the calculation - in the hydrohash(catchmentsIDs, list of hydrotopesIDs)
    final List< ? > catchmentList = (List< ? >) iniValuesRootFeature.getProperty( NaModelConstants.INI_CATCHMENT_MEMBER_PROP );
    final Set<String> catchmentIdsFromLzsim = hydroHash.getCatchmentIdsFromLzsim();
    for( final String catchmentID : catchmentIdsFromLzsim )
    {
      final StringBuffer lzsBuffer = new StringBuffer();

      final Feature naCatchmentFE = catchmentIDToFeatureHash.get( catchmentID );
      // in the catchmentIDToFeatureHash (HashMap<featureID, feature>) are all channels in the model. if this run is
      // only
      // a subnet of the total model the idManager only knows the featuers in the submodel just skip this one.
      if( naCatchmentFE == null )
        continue;
      final int asciiCatchmentID = idManager.getAsciiID( naCatchmentFE );
      final String fileName = "we" + asciiCatchmentID + ".lzs"; //$NON-NLS-1$//$NON-NLS-2$

      final File lzsFile = new File( lzsimDir, fileName );
      final List<String> sortedHydrosIDsfromLzsim = hydroHash.getSortedHydrosIDsfromLzsim( catchmentID );
      // find catchmentID in the iniValues
      for( int i = 0; i < catchmentList.size(); i++ )
      {
        final Feature catchmentFE = (Feature) catchmentList.get( i );
        // write lzs for the catchment
        if( catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ) ).equals( catchmentID ) ) //$NON-NLS-1$
        {
          try
          {
            // snow
            lzsBuffer.append( iniDate + " h     1 snow" + "\n" ); //$NON-NLS-1$//$NON-NLS-2$
            final Double h = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "h" ) ); //$NON-NLS-1$
            final Double ws = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "ws" ) ); //$NON-NLS-1$
            lzsBuffer.append( "   1" + FortranFormatHelper.printf( h, "f9.2" ) + FortranFormatHelper.printf( ws, "f9.2" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            // groundwater
            lzsBuffer.append( iniDate + " h     1 gwsp" + "\n" ); //$NON-NLS-1$//$NON-NLS-2$
            final Double hgws = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "hgws" ) ); //$NON-NLS-1$
            final Double qb = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "qb" ) ); //$NON-NLS-1$
            lzsBuffer.append( "   1" + FortranFormatHelper.printf( hgws, "f9.2" ) + FortranFormatHelper.printf( qb, "f9.2" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            // hydrotops (interception storage content& soil moisture)
            int hydroPos = 0;
            final List< ? > iniHydsList = (List< ? >) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "hyd" ) ); //$NON-NLS-1$
            lzsBuffer.append( iniDate + " h  " + FortranFormatHelper.printf( Integer.toString( iniHydsList.size() ), "i4" ) + " bodf" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$//$NON-NLS-4$
            for( final String hydroID : sortedHydrosIDsfromLzsim )
            {
              hydroPos++;
              final Iterator< ? > iter = iniHydsList.iterator();
              while( iter.hasNext() )
              {
                final Feature iniHydFe = (Feature) iter.next();
                // write initial parameters for the hydrotop
                final String hydFeatureId = (String) iniHydFe.getProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ) ); //$NON-NLS-1$
                if( hydFeatureId.equals( hydroID ) )
                {
                  final Double bi = (Double) iniHydFe.getProperty( new QName( NaModelConstants.NS_INIVALUES, "bi" ) ); //$NON-NLS-1$
                  lzsBuffer.append( FortranFormatHelper.printf( hydroPos, "i4" ) + FortranFormatHelper.printf( bi, "f7.2" ) ); //$NON-NLS-1$//$NON-NLS-2$
                  final List< ? > bofs = (List< ? >) iniHydFe.getProperty( new QName( NaModelConstants.NS_INIVALUES, "bofs" ) ); //$NON-NLS-1$
                  for( final Object bof : bofs )
                  {
                    lzsBuffer.append( FortranFormatHelper.printf( (Double) bof, "f7.2" ) ); //$NON-NLS-1$
                  }
                  lzsBuffer.append( "\n" ); //$NON-NLS-1$
                }
              }
            }
            final Writer lzsWriter = new FileWriter( lzsFile );
            lzsWriter.write( lzsBuffer.toString() );
            lzsWriter.close();
          }
          catch( final Exception e )
          {
            System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.87", catchmentFE.getId() ) ); //$NON-NLS-1$
          }
        }
      }
    }
  }
}