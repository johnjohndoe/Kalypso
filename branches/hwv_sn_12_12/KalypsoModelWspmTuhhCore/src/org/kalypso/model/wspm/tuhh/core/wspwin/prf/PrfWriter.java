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
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.AbstractProfileObject;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.BuildingUtil;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_GERINNE_ART;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_KENNUNG;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.DoubleDataBlock;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * Exports one {@link org.kalypso.model.wspm.core.profil.IProfil} as {@link org.kalypso.wspwin.core.prf.DataBlockWriter}
 * .
 * 
 * @author Gernot Belger
 */
public class PrfWriter implements IPrfConstants
{
  private final Map<Integer, String[]> m_defaultPrfMetadata = new HashMap<Integer, String[]>();

  private final DataBlockWriter m_dbWriter = new DataBlockWriter();

  private final IProfil m_profil;

  private final IWaterlevel[] m_waterlevels;

  private final String m_defaultRoughnessType;

  public PrfWriter( final IProfil profil, final IWaterlevel[] waterlevels )
  {
    this( profil, waterlevels, "" ); //$NON-NLS-1$
  }

  public PrfWriter( final IProfil profil, final IWaterlevel[] waterlevels, final String defaultRoughnessType )
  {
    m_defaultRoughnessType = defaultRoughnessType;
    m_profil = profil;
    m_waterlevels = waterlevels;
    fillDefaultPrfMetadata();
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
    m_defaultPrfMetadata.put( PRF_LINE_13_ZEICHNUNGSUEBERSCHRIFT, new String[] { "Zeichnungs¸berschrift", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setPrfMetadata( final int line, final String... value )
  {
    m_defaultPrfMetadata.put( line, value );
  }

  public DataBlockWriter export( )
  {
    extractMetaData();

    if( m_profil.getPoints().length > 0 )
      extractDataBlocks();

    return m_dbWriter;
  }

  private void extractMetaData( )
  {
    for( int i = 1; i <= 13; i++ )
    {
      // Skip station
      if( i == 9 )
        continue;

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

  private String toDataBlockKey( final Object profilKey )
  {
    if( IWspmTuhhConstants.WEHR_TYP_BEIWERT.equals( profilKey ) )
      return "BEIWERT"; //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG.equals( profilKey ) )
      return "RUNDKRONIG"; //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG.equals( profilKey ) )
      return "SCHARFKANTIG"; //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG.equals( profilKey ) )
      return "BREITKRONIG"; //$NON-NLS-1$
    else
      return profilKey.toString();
  }

  private void extractDataBlocks( )
  {
    writePoints();
    writeDevider();
    writeRauheit();

    // FIXME: spezial Zeugs f¸r Steiermark, aber wohin?
    writeWaterlevel();

    if( !ArrayUtils.isEmpty( m_profil.getProfileObjects( AbstractProfileObject.class ) ) )
      writeProfileObjects();
    if( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) != null )
      writeHochRechts();
    if( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) != null )
      writeBewuchs();
    if( m_profil.getComment() != null )
      writeComment();
  }

  private void writeWaterlevel( )
  {
    /* Case 1: waterlevels contained inside the profile (this is rarely the case, used actually only for 2d-waterlevels) */
    final ProfileWaterlevelWriter plotterExporter = new ProfileWaterlevelWriter( m_profil );
    final IDataBlock[] dbs1 = plotterExporter.createDataBlocks();
    for( final IDataBlock dataBlock : dbs1 )
      m_dbWriter.addDataBlock( dataBlock );

    /* Case 2: waterlevels obtained from results */
    final WaterlevelWriter waterlevelWriter = new WaterlevelWriter( m_profil, m_waterlevels );
    final IDataBlock[] dbs2 = waterlevelWriter.createDataBlocks();
    for( final IDataBlock dataBlock : dbs2 )
      m_dbWriter.addDataBlock( dataBlock );
  }

  private void writeComment( )
  {
    final String comment = m_profil.getComment();
    final DataBlockHeader dbh = createHeader(IWspmConstants.POINT_PROPERTY_COMMENT); //$NON-NLS-1$
    final TextDataBlock db = new TextDataBlock( dbh );

    final StringReader stringReader = new StringReader( comment );
    final LineNumberReader lineNumberReader = new LineNumberReader( stringReader );

    try
    {
      for( String line = lineNumberReader.readLine(); line != null; line = lineNumberReader.readLine() )
        db.addLine( "CC " + line ); //$NON-NLS-1$
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
    final DataBlockHeader dbh = createHeader( IWspmConstants.POINT_PROPERTY_HOEHE ); //$NON-NLS-1$
    final CoordDataBlock db = new CoordDataBlock( dbh );
    writeCoords( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ), db, null );
    m_dbWriter.addDataBlock( db );
  }

  private final boolean istDurchlass( final IProfileObject[] objects )
  {
    if( objects == null || objects.length < 1 )
      return false;
    final String building = objects[0].getId();
    if( building.equals( IWspmTuhhConstants.BUILDING_TYP_EI ) || building.equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) || building.equals( IWspmTuhhConstants.BUILDING_TYP_KREIS )
        || building.equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
      return true;
    return false;

  }

  private final IComponent[] getRoughness( )
  {
    final IComponent comp = m_profil.hasPointProperty( m_defaultRoughnessType );
    if( comp != null )
      return new IComponent[] { comp };
    final IComponent rks = m_profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent rkst = m_profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( rks == null && rkst == null )
      return new IComponent[] {};
    return new IComponent[] { rks, rkst };
  }

  private final Double getRoughnessFromBuilding( )
  {
    final IProfileBuilding[] buildings = m_profil.getProfileObjects( IProfileBuilding.class );
    if( !istDurchlass( buildings ) )
      return Double.NaN;
    return BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, buildings[0] );
  }

  private void writeRauheit( )
  {
    CoordDataBlock dbr = null;
    final Double buildingRoughness = getRoughnessFromBuilding();
    final IComponent[] cmpR = getRoughness();
    if( cmpR.length == 0 )
    {
      final DataBlockHeader dbhr = createHeader( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ); //$NON-NLS-1$
      dbr = new CoordDataBlock( dbhr );
      writeCoords( null, dbr, buildingRoughness.isNaN() ? 0.0 : buildingRoughness );
      if( !buildingRoughness.isNaN() )
        dbr.getY()[0] = buildingRoughness;
      m_dbWriter.addDataBlock( dbr );
    }
    for( final IComponent c : cmpR )
    {
      if( c == null )
        continue;
      final DataBlockHeader dbhr = createHeader( c.getId() ); //$NON-NLS-1$
      dbr = new CoordDataBlock( dbhr );
      writeCoords( c, dbr, buildingRoughness.isNaN() ? 0.0 : buildingRoughness );
      if( !buildingRoughness.isNaN() )
        dbr.getY()[0] = buildingRoughness;
      m_dbWriter.addDataBlock( dbr );
    }

  }

  private final DataBlockHeader createHeader( final String key )
  {
    if( IWspmTuhhConstants.POINT_PROPERTY_HOEHE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "GELAENDE-", "HOEHE" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "TRENNFLAECHEN" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "DURCHSTROEMTE", "BEREICHE" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "BORDVOLL" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "TRENNLINIE", "WEHR" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "RAUHEIT", "kst   m" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS.equals( key ) )
      return new DataBlockHeader( "RAUHEIT", "k-s   m" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "RECHTSWERT" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "HOCHWERT" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "UK-BRUECKE" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "OK-BRUECKE" ); //$NON-NLS-1$

    if( IWspmConstants.POINT_PROPERTY_COMMENT.equals( key )) //$NON-NLS-1$
    {
      // REMARK: Important: Kommmentar MUST be written with lower case letters, else WspWin will not read it...
      return new DataBlockHeader( "Kommentar:" ); //$NON-NLS-1$
    }

    if( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "AX   m" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "AY   m" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "DP   m" ); //$NON-NLS-1$

    if( IWspmTuhhConstants.BUILDING_TYP_EI.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "EI", "", IWspWinConstants.SPEZIALPROFIL_EI ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.BUILDING_TYP_KREIS.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "KREIS", "", IWspWinConstants.SPEZIALPROFIL_KREIS ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "TRAPEZ", "", IWspWinConstants.SPEZIALPROFIL_TRAPEZ ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.BUILDING_TYP_MAUL.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "MAULPROFIL", "", IWspWinConstants.SPEZIALPROFIL_MAUL ); //$NON-NLS-1$ //$NON-NLS-2$

    if( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR.equals( key ) ) //$NON-NLS-1$
      return new DataBlockHeader( "OK-WEHR" ); //$NON-NLS-1$

    if( ISinuositaetProfileObject.ID.equals( key ) )
      return new DataBlockHeader( "SINUOSITAET", "[-]", IWspWinConstants.SPEZIALPROFIL_SINUOSITAET );//$NON-NLS-1$ //$NON-NLS-2$

    // FIXME: we should probably throw an exception here: finding the error afterwards is very difficult
    return new DataBlockHeader( key );
  }


  private void writeCoords( final IComponent prop, final CoordDataBlock db, final Double nullValue )
  {
    final IRecord[] points = m_profil.getPoints();

    final List<Double> xs = new ArrayList<Double>( points.length );
    final List<Double> ys = new ArrayList<Double>( points.length );

    final int iBreite = m_profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iProp = m_profil.indexOfProperty( prop );
    for( final IRecord point : points )
    {
      final Double x = (Double) point.getValue( iBreite );

      final Double value = iProp < 0 ? null : (Double) point.getValue( iProp );
      if( value == null )
      {
        if( nullValue != null )
        {
          xs.add( x );
          ys.add( nullValue );
        }
      }
      else
      {
        xs.add( x );
        ys.add( value );
      }
    }

    final Double[] xArray = xs.toArray( new Double[xs.size()] );
    final Double[] yArray = ys.toArray( new Double[ys.size()] );
    db.setCoords( xArray, yArray );
  }

  private void writeHochRechts( )
  {
    final DataBlockHeader dbhh = createHeader( IWspmConstants.POINT_PROPERTY_HOCHWERT ); //$NON-NLS-1$
    final CoordDataBlock dbh = new CoordDataBlock( dbhh );
    writeCoords( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ), dbh, null );
    m_dbWriter.addDataBlock( dbh );

    final DataBlockHeader dbhr = createHeader( IWspmConstants.POINT_PROPERTY_RECHTSWERT ); //$NON-NLS-1$
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    writeCoords( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), dbr, null );
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
    final IProfilPointMarker[] deviders = m_profil.getPointMarkerFor( m_profil.hasPointProperty( key ) );
    if( deviders == null || deviders.length == 0 )
      return;

    final CoordDataBlock dbw = new CoordDataBlock( createHeader( key ) );
    final Double[] xs = new Double[deviders.length];
    final Double[] ys = new Double[deviders.length];
    final int iBreite = m_profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    for( int i = 0; i < deviders.length; i++ )
    {
      final IProfilPointMarker devider = deviders[i];
      final IRecord point = devider.getPoint();

      try
      {
        xs[i] = (Double) point.getValue( iBreite );
        ys[i] = getDeviderYValue( devider, i );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.21", devider.getId().toString() ), e ) ); //$NON-NLS-1$
      }
    }
    dbw.setCoords( xs, ys );
    m_dbWriter.addDataBlock( dbw );
  }

  /**
   * Get coodinate y value: depends on type of marker.
   */
  private double getDeviderYValue( final IProfilPointMarker devider, final int index )
  {
    final IComponent markerId = devider.getId();

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId.getId() ) )
    {
      final boolean isBoeschung = devider.getIntepretedValue() == null ? false : (Boolean) devider.getIntepretedValue();
      final int offset = isBoeschung ? 3 : 1;
      return offset + index;
    }

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId.getId() ) )
      return index + 1;
    final int iHoehe = m_profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    return (Double) devider.getPoint().getValue( iHoehe );
  }

  private void writeProfileObjects( )
  {
    final List<IProfileBuilding> buildings = new ArrayList<IProfileBuilding>();

    final AbstractProfileObject[] profileObjects = m_profil.getProfileObjects( AbstractProfileObject.class );
    for( final AbstractProfileObject profileObject : profileObjects )
    {
      if( profileObject instanceof IProfileBuilding )
      {
        buildings.add( (IProfileBuilding) profileObject );
      }
      else
      {
        writeProfileObject( profileObject );
      }
    }

    /**
     * tuhh profile restriction - only one profile building allowed!
     */
    if( !buildings.isEmpty() )
      writeBuilding( buildings.get( 0 ) );

  }

  private void writeBuilding( final IProfileBuilding building )
  {
    final String buildingType = building.getId();
    if( buildingType.equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) )
    {
      final DataBlockHeader dbho = createHeader( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ); //$NON-NLS-1$
      final CoordDataBlock dbo = new CoordDataBlock( dbho );
      writeCoords( m_profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), dbo, null );
      m_dbWriter.addDataBlock( dbo );
      final DataBlockHeader dbhu = createHeader( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ); //$NON-NLS-1$
      final CoordDataBlock dbu = new CoordDataBlock( dbhu );
      writeCoords( m_profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), dbu, null );
      try
      {
        final String secLine = String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER ) ) //$NON-NLS-1$
        + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) //$NON-NLS-1$
        + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) ) //$NON-NLS-1$
        + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ); //$NON-NLS-1$
        dbu.setSecondLine( secLine );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.30" ), e ) ); //$NON-NLS-1$
      }
      m_dbWriter.addDataBlock( dbu );
    }
    else if( buildingType.compareTo( IWspmTuhhConstants.BUILDING_TYP_WEHR ) == 0 )
    {
      final DataBlockHeader dbhw = createHeader( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$
      final CoordDataBlock dbw = new CoordDataBlock( dbhw );
      writeCoords( m_profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ), dbw, null );
      try
      {
        final Object wehrart = building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );

        final StringBuffer secLine = new StringBuffer( toDataBlockKey( wehrart ) );
        secLine.append( String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ) ); //$NON-NLS-1$
        final IProfilPointMarker[] deviders = m_profil.getPointMarkerFor( m_profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
        for( final IProfilPointMarker devider : deviders )
          secLine.append( String.format( Locale.US, " %12.4f", devider.getValue() ) ); //$NON-NLS-1$
        dbw.setSecondLine( secLine.toString() );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.34" ), e ) ); //$NON-NLS-1$
      }
      m_dbWriter.addDataBlock( dbw );
    }
    else if( IWspmTuhhConstants.BUILDING_TYP_EI.equals( buildingType ) )
    {
      final DataBlockHeader dbhe = createHeader( IWspmTuhhConstants.BUILDING_TYP_EI );
      final DoubleDataBlock dbe = new DoubleDataBlock( dbhe );
      try
      {
        dbe.setDoubles( new Double[] { (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) } );
      }

      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.37" ), e ) ); //$NON-NLS-1$
      }
      m_dbWriter.addDataBlock( dbe );
    }
    else if( IWspmTuhhConstants.BUILDING_TYP_MAUL.equals( buildingType ) )
    {
      final DataBlockHeader dbhm = createHeader( buildingType );
      final DoubleDataBlock dbm = new DoubleDataBlock( dbhm );

      try
      {
        dbm.setDoubles( new Double[] { (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) } );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.40" ), e ) ); //$NON-NLS-1$
      }
      m_dbWriter.addDataBlock( dbm );
    }
    else if( IWspmTuhhConstants.BUILDING_TYP_KREIS.equals( buildingType ) )
    {
      final DataBlockHeader dbhk = createHeader( buildingType );
      final DoubleDataBlock dbk = new DoubleDataBlock( dbhk );

      try
      {
        dbk.setDoubles( new Double[] { (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) } );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.43" ), e ) ); //$NON-NLS-1$
      }
      m_dbWriter.addDataBlock( dbk );
    }

    else if( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ.equals( buildingType ) )
    {
      final DataBlockHeader dbht = createHeader( buildingType ); //$NON-NLS-1$
      final DoubleDataBlock dbt = new DoubleDataBlock( dbht );
      try
      {
        dbt.setDoubles( new Double[] { (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ),
            (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ), (Double) building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) } );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.46" ), e ) ); //$NON-NLS-1$
      }
      m_dbWriter.addDataBlock( dbt );
    }

  }

  private void writeProfileObject( final IProfileObject profileObject )
  {
    if( profileObject instanceof ISinuositaetProfileObject )
    {
      final ISinuositaetProfileObject sinuosity = (ISinuositaetProfileObject) profileObject;

      final DataBlockHeader header = createHeader( sinuosity.getId() ); 
      final DoubleDataBlock dataBlock = new DoubleDataBlock( header );
      final SINUOSITAET_KENNUNG kennung = sinuosity.getKennung();
      final double sinus = sinuosity.getSinuositaet();
      final SINUOSITAET_GERINNE_ART gerinne = sinuosity.getGerinneArt();
      final double lf = sinuosity.getLf();

      dataBlock.setDoubles( new Double[] { new Double( kennung.toInteger() ), sinus, new Double( gerinne.toInteger() ), lf } );

      m_dbWriter.addDataBlock( dataBlock );
    }

  }

  private void writeBewuchs( )
  {
    final DataBlockHeader dbhx = createHeader( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
    final CoordDataBlock dbx = new CoordDataBlock( dbhx );
    final DataBlockHeader dbhy = createHeader( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
    final CoordDataBlock dby = new CoordDataBlock( dbhy );
    final DataBlockHeader dbhp = createHeader( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
    final CoordDataBlock dbp = new CoordDataBlock( dbhp );
    writeCoords( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ), dbx, 0.0 );
    writeCoords( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ), dby, 0.0 );
    writeCoords( m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ), dbp, 0.0 );
    m_dbWriter.addDataBlock( dbx );
    m_dbWriter.addDataBlock( dby );
    m_dbWriter.addDataBlock( dbp );
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
