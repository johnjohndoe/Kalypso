/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * Exports one {@link org.kalypso.model.wspm.core.profil.IProfil} as {@link org.kalypso.wspwin.core.prf.DataBlockWriter}
 * 
 * @author Gernot Belger
 */
public class PrfWriter implements IPrfConstants
{
  private final Map<Integer, String[]> m_defaultPrfMetadata = new HashMap<>();

  private final DataBlockWriter m_dbWriter = new DataBlockWriter();

  private final IProfile m_profil;

  private final IWaterlevel[] m_waterlevels;

  private final PrfRoughnessWriter m_roughnessWriter;

  private final PrfVegetationWriter m_vegetationWriter;

  public PrfWriter( final IProfile profil, final IWaterlevel[] waterlevels, final String defaultRoughnessType, final boolean preferRoughnessClasses, final boolean preferVegetationClasses )
  {
    m_profil = profil;
    m_waterlevels = waterlevels;
    fillDefaultPrfMetadata();

    m_roughnessWriter = new PrfRoughnessWriter( m_dbWriter, profil, defaultRoughnessType, preferRoughnessClasses );
    m_vegetationWriter = new PrfVegetationWriter( m_dbWriter, profil, preferVegetationClasses );
  }

  private void fillDefaultPrfMetadata( )
  {
    /* Default values */
    m_defaultPrfMetadata.put( PRF_LINE_1_URDATEI, new String[] { "Urdatei", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 2, new String[] { "Auftraggeber 1", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 3, new String[] { "Auftraggeber 2", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( PRF_LINE_4_PROJEKTBEZEICHNUNG_1, new String[] { "Projektbezeichnung 1", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( PRF_LINE_5_PROJEKTBEZEICHNUNG_2, new String[] { "Projektbezeichnung 2", "0" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( PRF_LINE_6_PROJEKTBEZEICHNUNG_3, new String[] { "Projektbezeichnung 3", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 7, new String[] { "Blattbezeichnung 1", "0" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 8, new String[] { "Blattbezeichnung 2", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    // 9: former Blattbezeichnung 3: now 'Station'
    // DEFAULT_PRF_METADATA.put( 9, new String[] { "Blattbezeichnung 3", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 10, new String[] { "Projektnummer", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 11, new String[] { "Datum", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( 12, new String[] { "Blattnummer", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    m_defaultPrfMetadata.put( PRF_LINE_13_ZEICHNUNGSUEBERSCHRIFT, new String[] { "Zeichnungsüberschrift", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setPrfMetadata( final int line, final String... values )
  {
    final String[] cleanValues = new String[values.length];
    for( int i = 0; i < cleanValues.length; i++ )
      cleanValues[i] = cleanCarriageReturn( values[i] );

    m_defaultPrfMetadata.put( line, cleanValues );
  }

  private static String cleanCarriageReturn( final String value )
  {
    if( value == null )
      return StringUtils.EMPTY;

    return value.replace( '\n', '-' ).replace( '\r', '-' );
  }

  public DataBlockWriter export( )
  {
    extractMetaData();

    if( m_profil.getPoints().length > 0 )
    {
      extractDataBlocks();
    }

    return m_dbWriter;
  }

  private void extractMetaData( )
  {
    for( int i = 1; i <= 13; i++ )
    {
      // Skip station
      if( i == 9 )
      {
        continue;
      }

      final String[] line = getPrfMetadata( i );
      m_dbWriter.addKeyValue( i, line );
    }

    // Alte Defaults: wichtig?
//     line3: new String[] { "STATUS", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
//     line5: new String[] { "VERZWEIGUNGSKENNUNG", "0" } ); //$NON-NLS-1$ //$NON-NLS-2$
//     line6:  new String[] { "WASSERSPIEGEL", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
//     line7: new String[] { "MEHRFELDBRUECKE", "0" } ); //$NON-NLS-1$ //$NON-NLS-2$

    final double d = m_profil.getStation();
    final double station = Double.isNaN( d ) ? -9999.9999 : d;
    m_dbWriter.addKeyValue( 9, new String[] { String.format( Locale.US, "STATION KM %.4f", station ), "" } ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private String[] getPrfMetadata( final int i )
  {
    // FIXME: should also try to fetch from metadata, but thatgets never written into the features at the moment, so
    // this makes no sense.

    final String[] defaultStrings = m_defaultPrfMetadata.get( i );
    if( defaultStrings != null )
      return defaultStrings;

    return new String[] { "", "" };//$NON-NLS-1$ //$NON-NLS-2$
  }

  private void extractDataBlocks( )
  {
    writePoints();
    writeDevider();

    m_roughnessWriter.writeRauheit();

    // FIXME: spezial Zeugs für Steiermark, aber wohin?
    writeWaterlevel();

    /* write profile objects as special datablocks */
    new ProfileObjectsPrfWriter( m_profil, m_dbWriter ).write();

    if( m_profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ) != null )
      writeHochRechts();

    m_vegetationWriter.writeBewuchs();

    if( m_profil.getComment() != null )
      writeComment();
  }

  private void writeWaterlevel( )
  {
    /* Case 1: waterlevels contained inside the profile (this is rarely the case, used actually only for 2d-waterlevels) */
    final ProfileWaterlevelWriter plotterExporter = new ProfileWaterlevelWriter( m_profil );
    final IDataBlock[] dbs1 = plotterExporter.createDataBlocks();
    for( final IDataBlock dataBlock : dbs1 )
    {
      m_dbWriter.addDataBlock( dataBlock );
    }

    /* Case 2: waterlevels obtained from results */
    final WaterlevelWriter waterlevelWriter = new WaterlevelWriter( m_profil, m_waterlevels );
    final IDataBlock[] dbs2 = waterlevelWriter.createDataBlocks();
    for( final IDataBlock dataBlock : dbs2 )
    {
      m_dbWriter.addDataBlock( dataBlock );
    }
  }

  private void writeComment( )
  {
    final String comment = m_profil.getComment();
    final DataBlockHeader dbh = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_COMMENT ); //$NON-NLS-1$
    final TextDataBlock db = new TextDataBlock( dbh );

    final StringReader stringReader = new StringReader( comment );
    final LineNumberReader lineNumberReader = new LineNumberReader( stringReader );

    try
    {
      for( String line = lineNumberReader.readLine(); line != null; line = lineNumberReader.readLine() )
      {
        db.addLine( "CC " + line ); //$NON-NLS-1$
      }
      if( db.getCoordCount() > 0 )
      {
        db.setThirdLine( "0  0  0  0  0  0  0  " + Integer.toString( db.getCoordCount() ) + " " + IWspWinConstants.SPEZIALPROFIL_COMMENT ); //$NON-NLS-1$ //$NON-NLS-2$
        m_dbWriter.addDataBlock( db );
      }
    }
    catch( final IOException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.8" ), e ) ); //$NON-NLS-1$
    }
  }

  private void writePoints( )
  {
    final DataBlockHeader dbh = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_HOEHE ); //$NON-NLS-1$
    final CoordDataBlock db = writeCoords( m_profil, IWspmPointProperties.POINT_PROPERTY_HOEHE, dbh, null );
    m_dbWriter.addDataBlock( db );
  }

  /**
   * Creates a CoordDataBlock for a given component.
   */
  static final CoordDataBlock writeCoords( final IProfile profil, final String componentID, final DataBlockHeader dbh, final Double nullValue )
  {
    final IRecord[] points = profil.getPoints();

    final List<Double> xs = new ArrayList<>( points.length );
    final List<Double> ys = new ArrayList<>( points.length );

    final int iBreite = profil.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );
    final int iProp = profil.indexOfProperty( componentID );
    for( final IRecord point : points )
    {
      final Double width = (Double)point.getValue( iBreite );
      if( width == null )
        continue;

      final Double value = iProp < 0 ? null : (Double)point.getValue( iProp );
      if( value == null )
      {
        if( nullValue != null )
        {
          xs.add( width );
          ys.add( nullValue );
        }
      }
      else
      {
        xs.add( width );
        ys.add( value );
      }
    }

    final Double[] xArray = xs.toArray( new Double[xs.size()] );
    final Double[] yArray = ys.toArray( new Double[ys.size()] );

    final CoordDataBlock db = new CoordDataBlock( dbh );
    db.setCoords( xArray, yArray );
    return db;
  }

  private void writeHochRechts( )
  {
    final DataBlockHeader dbhh = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ); //$NON-NLS-1$
    final CoordDataBlock dbh = writeCoords( m_profil, IWspmPointProperties.POINT_PROPERTY_HOCHWERT, dbhh, null );
    m_dbWriter.addDataBlock( dbh );

    final DataBlockHeader dbhr = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_RECHTSWERT ); //$NON-NLS-1$
    final CoordDataBlock dbr = writeCoords( m_profil, IWspmPointProperties.POINT_PROPERTY_RECHTSWERT, dbhr, null );
    m_dbWriter.addDataBlock( dbr );
  }

  private void writeDevider( )
  {
    writeDeviderTyp( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ); //$NON-NLS-1$
    writeDeviderTyp( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ); //$NON-NLS-1$
    writeDeviderTyp( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ); //$NON-NLS-1$
    writeDeviderTyp( IWspmTuhhConstants.MARKER_TYP_WEHR ); //$NON-NLS-1$
  }

  private void writeDeviderTyp( final String key )
  {
    final IProfilePointMarker[] deviders = m_profil.getPointMarkerFor( m_profil.hasPointProperty( key ) );
    if( deviders == null || deviders.length == 0 )
      return;

    final CoordDataBlock dbw = new CoordDataBlock( PrfHeaders.createHeader( key ) );
    final Double[] xs = new Double[deviders.length];
    final Double[] ys = new Double[deviders.length];
    final int iBreite = m_profil.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );
    for( int i = 0; i < deviders.length; i++ )
    {
      final IProfilePointMarker devider = deviders[i];
      final IRecord point = devider.getPoint();

      try
      {
        xs[i] = (Double)point.getValue( iBreite );
        ys[i] = getDeviderYValue( devider, i );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.21", devider.getComponent().toString() ), e ) ); //$NON-NLS-1$
      }
    }
    dbw.setCoords( xs, ys );
    m_dbWriter.addDataBlock( dbw );
  }

  /**
   * Get coodinate y value: depends on type of marker.
   */
  private double getDeviderYValue( final IProfilePointMarker devider, final int index )
  {
    final IComponent markerId = devider.getComponent();

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId.getId() ) )
    {
      final boolean isBoeschung = devider.getIntepretedValue() == null ? false : (Boolean)devider.getIntepretedValue();
      final int offset = isBoeschung ? 3 : 1;
      return offset + index;
    }

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId.getId() ) )
      return index + 1;
    final int iHoehe = m_profil.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE );
    return (Double)devider.getPoint().getValue( iHoehe );
  }

  public void write( final Writer writer ) throws IOException
  {
    final DataBlockWriter dbWriter = export();
    dbWriter.store( new PrintWriter( writer ) );
  }

  public void write( final File file ) throws IOException
  {
    Writer writer = null;
    try
    {
      writer = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( file ) ) );
      write( writer );
      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}