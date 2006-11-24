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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.math.Range;
import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.wspwin.core.prf.PrfReader;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

/**
 * @author kimwerner
 */
public class PrfSource implements IProfilSource
{
  private final static Logger m_logger = Logger.getLogger( PrfSource.class.getName() );

  private void readSource( final PrfReader pr, final IProfil p )
  {
    if( p == null )
      return;
    try
    {
      p.setProperty( "prfFileFormat_MetaData", pr.getMetaData() );
      p.setProperty( PROFIL_PROPERTY.STATUS, pr.getKeyValue( 3 )[1] );
      p.setProperty( PROFIL_PROPERTY.VERZWEIGUNGSKENNUNG, pr.getKeyValue( 5 )[1] );
      p.setProperty( PROFIL_PROPERTY.WASSERSPIEGEL, pr.getKeyValue( 6 )[1] );
      p.setProperty( PROFIL_PROPERTY.MEHRFELDBRUECKE, pr.getKeyValue( 7 )[1] );
      final String stat = pr.getKeyValue( 9 )[0];
      if( stat.startsWith( "STATION " ) )
        p.setStation( new Double( stat.substring( 10 ) ) );
      if( readPoints( p, pr ) > 0 )
      {
        readTrennFl( p, pr );
        readDurchStr( p, pr );
        readBordVoll( p, pr );
        readRauhheit( p, pr );
        readBewuchs( p, pr );
        readComment( p, pr );
        readGeoCoord( p, pr );
        if( !(readWehr( p, pr ) || readBridge( p, pr )) )
          readBuilding( p, pr );
      }
    }
    catch( ProfilDataException e )
    {
      m_logger.log( Level.SEVERE, e.getMessage() );
      e.printStackTrace();
    }
  }

  private void readComment( IProfil p, PrfReader pr ) throws ProfilDataException
  {

    final IDataBlock db = pr.getDataBlock( "KOM" );
    if( db == null )
      return;
    ArrayList<String> sl = new ArrayList<String>();
    for( String line : db.getText() )
    {
      sl.add( line.substring( 3 ) );
    }
    p.setProperty( PROFIL_PROPERTY.KOMMENTAR, sl );
  }

  private void readBewuchs( IProfil p, PrfReader pr )
  {
    final IDataBlock dbx = pr.getDataBlock( "AX" );
    final IDataBlock dby = pr.getDataBlock( "AY" );
    final IDataBlock dbp = pr.getDataBlock( "DP" );
    if( dbx == null || dby == null || dbp == null )
      return;
    writePointProperty( p, POINT_PROPERTY.BEWUCHS_AX, dbx );
    writePointProperty( p, POINT_PROPERTY.BEWUCHS_AY, dby );
    writePointProperty( p, POINT_PROPERTY.BEWUCHS_DP, dbp );

  }

  private void writePointProperty( final IProfil p, final POINT_PROPERTY property, final IDataBlock db )
  {
    p.addPointProperty( property );
    final double[] xs = db.getX();
    final double[] ys = db.getY();
    for( int i = 0; i < xs.length; i++ )
    {
      final IProfilPoint point = ProfilUtil.findPoint( p, i, xs[i], 0 );
      if( point != null )
        point.setValueFor( property, ys[i] );
    }
  }

  private void readGeoCoord( IProfil p, PrfReader pr )
  {
    final IDataBlock dbh = pr.getDataBlock( "HOC" );
    final IDataBlock dbr = pr.getDataBlock( "REC" );
    if( dbh == null || dbr == null )
      return;
    writePointProperty( p, POINT_PROPERTY.HOCHWERT, dbh );
    writePointProperty( p, POINT_PROPERTY.RECHTSWERT, dbr );
  }

  private void readBuilding( IProfil p, PrfReader pr ) throws ProfilDataException
  {
    IDataBlock db = pr.getDataBlock( "EI" );
    if( db == null )
      db = pr.getDataBlock( "TRA" );
    if( db == null )
      db = pr.getDataBlock( "KRE" );
    if( db == null )
      db = pr.getDataBlock( "MAU" );
    if( db == null )
      return;
    final DataBlockHeader dbh = db.getDataBlockHeader();
    final String[] value = db.getText();
    if( value != null && value.length > 0 )
    {
      final StringTokenizer sT = new StringTokenizer( value[0], " " );
      double rauheit = 0.0;
      IDataBlock dbRau = pr.getDataBlock( "RAU" );
      if( dbRau != null && dbRau.getY().length > 0 )
        rauheit = dbRau.getY()[0];
      switch( dbh.getSpecification( 8 ) )
      {
        case 6:// Trapez
        {
          final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( IProfilConstants.BUILDING_TYP_TRAPEZ );
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BREITE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.HOEHE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.STEIGUNG ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.SOHLGEFAELLE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_Y ) )
            return;
          building.setValue( BUILDING_PROPERTY.RAUHEIT, rauheit );
          p.setBuilding( building );
          break;
        }
        case 7:// Kreis
        {
          final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding(  IProfilConstants.BUILDING_TYP_KREIS );
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BREITE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.SOHLGEFAELLE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_Y ) )
            return;
          building.setValue( BUILDING_PROPERTY.RAUHEIT, rauheit );
          p.setBuilding( building );
          break;
        }
        case 8:// Ei
        {
          final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding(  IProfilConstants.BUILDING_TYP_EI );
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BREITE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.HOEHE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.SOHLGEFAELLE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_Y ) )
            return;
          building.setValue( BUILDING_PROPERTY.RAUHEIT, rauheit );
          p.setBuilding( building );
          break;
        }
        case 9:// Maulprofil
        {
          final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding(  IProfilConstants.BUILDING_TYP_MAUL );
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BREITE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.HOEHE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.SOHLGEFAELLE ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_X ) )
            return;
          if( !writeBuildingProperty( building, sT, BUILDING_PROPERTY.BEZUGSPUNKT_Y ) )
            return;
          building.setValue( BUILDING_PROPERTY.RAUHEIT, rauheit );
          p.setBuilding( building );
          break;
        }

      }

    }
  }

  private final boolean writeBuildingProperty( final IProfilBuilding building, final StringTokenizer sT, final BUILDING_PROPERTY property )
  {
    if( sT.hasMoreTokens() )
    {
      try
      {
        building.setValue( property, Double.parseDouble( sT.nextToken() ) );
        return true;
      }
      catch( ProfilDataException e )
      {
        m_logger.log( Level.SEVERE, e.getMessage() );
        return false;
      }

    }
    else
      m_logger.log( Level.SEVERE, "Fehler beim Lesen der Bauwerkseigenschaft: " + property.toString() );
    return false;
  }

  private boolean readBridge( IProfil p, PrfReader pr ) throws ProfilDataException
  {
    final IDataBlock dbo = pr.getDataBlock( "OK-B" );
    final IDataBlock dbu = pr.getDataBlock( "UK-B" );
    if( dbo == null || dbu == null )
      return false;

    final IProfilBuilding bridge = ProfilBuildingFactory.createProfilBuilding(  IProfilConstants.BUILDING_TYP_BRUECKE );
    final StringTokenizer sT = new StringTokenizer( dbu.getSecondLine(), " " );
    if( sT.countTokens() > 4 )
    {
      m_logger.log( Level.WARNING, "Ungültige Anzahl von Eigenschaften für die Brücke. Es werden nur die ersten Vier ausgewertet." );
    }
    writeBuildingProperty( bridge, sT, BUILDING_PROPERTY.UNTERWASSER );
    writeBuildingProperty( bridge, sT, BUILDING_PROPERTY.BREITE );
    writeBuildingProperty( bridge, sT, BUILDING_PROPERTY.RAUHEIT );
    writeBuildingProperty( bridge, sT, BUILDING_PROPERTY.FORMBEIWERT );
    final double delta = (Double) POINT_PROPERTY.OBERKANTEBRUECKE.getParameter( PARAMETER.PRECISION );
    p.setBuilding( bridge );
    final PolyLine polyLineO = new PolyLine( dbo.getX(), dbo.getY(), delta );
    final PolyLine polyLineU = new PolyLine( dbu.getX(), dbu.getY(), delta );
    final Range rangeO = new Range( polyLineO.getFirstX(), polyLineO.getLastX(), delta );
    final Range rangeU = new Range( polyLineU.getFirstX(), polyLineU.getLastX(), delta );

    for( final Iterator<IProfilPoint> points = p.getPoints().iterator(); points.hasNext(); )
    {
      final IProfilPoint point = points.next();
      final double breite = point.getValueFor( POINT_PROPERTY.BREITE );
      final double hoehe = point.getValueFor( POINT_PROPERTY.HOEHE );

      if( rangeO.contains( breite ) )
        point.setValueFor( POINT_PROPERTY.OBERKANTEBRUECKE, polyLineO.getYFor( breite ) );
      else
        point.setValueFor( POINT_PROPERTY.OBERKANTEBRUECKE, hoehe );
      if( rangeU.contains( breite ) )
        point.setValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE, polyLineU.getYFor( breite ) );
      else
        point.setValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE, hoehe );
    }
    return true;

  }

  private int readPoints( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "Gelae" );
    if( db == null )
      return 0;
    final double[] xs = db.getX();
    final double[] ys = db.getY();
    for( int i = 0; i < xs.length; i++ )
    {
      p.addPoint( xs[i], ys[i] );
    }
    return xs.length;
  }

  private void readRauhheit( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "RAU" );
    if( db == null )
      return;
    p.addPointProperty( POINT_PROPERTY.RAUHEIT );
    final String rks = db.getSecondLine().toUpperCase();
    try
    {
      if( rks.startsWith( "KST" ) )
      {
        p.setProperty( PROFIL_PROPERTY.RAUHEIT_TYP, IProfilConstants.RAUHEIT_TYP_KST );
      }
      else if( rks.startsWith( "KS" ) )
      {
        p.setProperty( PROFIL_PROPERTY.RAUHEIT_TYP, IProfilConstants.RAUHEIT_TYP_KS );
      }
    }
    catch( ProfilDataException e )
    {
      m_logger.log( Level.SEVERE, "Rauheitstyp konnte nicht geschrieben werden" );
    }

    for( int i = 0; i < db.getCoordCount(); i++ )
    {
      final IProfilPoint point = ProfilUtil.findPoint( p, i, db.getX()[i], 0 );
      if( point != null )
        point.setValueFor( POINT_PROPERTY.RAUHEIT, db.getY()[i] );
    }
  }

  private void readTrennFl( final IProfil p, final PrfReader pr ) throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = p.getPoints();
    if( points.isEmpty() )
      return;
    final IDataBlock db = pr.getDataBlock( "TRENNFLAECHEN" );
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IProfilPoint p1 = null;
    IProfilPoint p2 = null;
    int pos1 = 0;
    int pos2 = 0;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
      pos1 = (int) db.getY()[0];
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
      pos2 = (int) db.getY()[1];
    }
    if( pCount > 2 )
      m_logger.log( Level.INFO, "mehr als 2 Datensätze für Trennflächen können an Station(" + p.getStation() + ") nicht ausgewertet werden" );

    // TODO: KIM in den Reparator verschieben
    // --------------------
    if( p1 == null )
    {
      p1 = points.getFirst();
      pos1 = 0;
      m_logger.log( Level.INFO, "Erzeuge Trennfläche für Station(" + p.getStation() + ")  an Position [" + Double.toString( p1.getValueFor( POINT_PROPERTY.BREITE ) ) + "]" );
    }
    if( p2 == null )
    {
      p2 = points.getLast();
      pos2 = 0;
      m_logger.log( Level.INFO, "Erzeuge Trennfläche für Station(" + p.getStation() + ")  an Position [" + Double.toString( p2.getValueFor( POINT_PROPERTY.BREITE ) ) + "]" );
    }
    // --------------------
    if( p1 != null )
    {
      final IProfilDevider devider1 = p.addDevider( p1, DEVIDER_TYP.TRENNFLAECHE );
      devider1.setValueFor( DEVIDER_PROPERTY.BOESCHUNG, (pos1 == 3) );
    }
    if( p2 != null )
    {
      final IProfilDevider devider2 = p.addDevider( p2, DEVIDER_TYP.TRENNFLAECHE );
      devider2.setValueFor( DEVIDER_PROPERTY.BOESCHUNG, (pos2 == 4) );
    }
  }

  private void readDurchStr( final IProfil p, final PrfReader pr ) throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = p.getPoints();
    if( points.isEmpty() )
      return;
    final IDataBlock db = pr.getDataBlock( "DURCHSTROEMTE" );
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IProfilPoint p1 = null;
    IProfilPoint p2 = null;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
    }
    if( pCount > 2 )
      m_logger.log( Level.INFO, "mehr als 2 Datensätze an Station(" + p.getStation() + ")  für Durchströmte Bereiche können nicht ausgewertet werden" );
    // TODO: KIM in den Reparator verschieben
    // -------------
    if( p1 == null )
    {
      p1 = points.getFirst();
      m_logger.log( Level.INFO, "Erzeuge Durchströmten Bereich für Station(" + p.getStation() + ")  an Position [" + Double.toString( p1.getValueFor( POINT_PROPERTY.BREITE ) ) + "]" );
    }
    if( p2 == null )
    {
      p2 = points.getLast();
      m_logger.log( Level.INFO, "Erzeuge Durchströmten Bereich für Station(" + p.getStation() + ")  an Position [" + Double.toString( p2.getValueFor( POINT_PROPERTY.BREITE ) ) + "]" );
    }
    // ---------------
    if( p1 != null )
      p.addDevider( p1, DEVIDER_TYP.DURCHSTROEMTE );
    if( p2 != null )
      p.addDevider( p2, DEVIDER_TYP.DURCHSTROEMTE );
  }

  private void readWehrtrenner( final double[] values, final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbt = pr.getDataBlock( "TRENNLINIE" );
    if( dbt == null )
      return;
    final double[] pos = dbt.getX();
    for( int i = 0; i < pos.length; i++ )
    {
      final IProfilPoint point = ProfilUtil.findPoint( p, pos[i], 0 );
      if( point != null )
      {
        if( (values != null) && (values.length > i + 1) )
        {
          final IProfilDevider dev = p.addDevider( point, DEVIDER_TYP.WEHR );
          dev.setValueFor( DEVIDER_PROPERTY.BEIWERT, values[i + 1] );
        }
      }
    }
  }

  private boolean readWehr( final IProfil p, final PrfReader pr ) throws ProfilDataException
  {
    final IDataBlock dbw = pr.getDataBlock( "OK-WEHR" );
    if( dbw == null )
      return false;
    final IProfilBuilding wehr = ProfilBuildingFactory.createProfilBuilding(  IProfilConstants.BUILDING_TYP_WEHR );
    final String secLine = dbw.getSecondLine();
    final String wa = getWehrart( secLine );
    final double[] wt = getWehrParameter( secLine );
    if( wa != null )
      wehr.setValue( BUILDING_PROPERTY.WEHRART, wa );
    wehr.setValue( BUILDING_PROPERTY.FORMBEIWERT, wt == null ? 0.0 : wt[0] );
    p.setBuilding( wehr );
    readWehrtrenner( wt, p, pr );
    final double delta = (Double) POINT_PROPERTY.OBERKANTEWEHR.getParameter( PARAMETER.PRECISION );
    final PolyLine polyLineO = new PolyLine( dbw.getX(), dbw.getY(), delta );
    final Range rangeO = new Range( polyLineO.getFirstX(), polyLineO.getLastX(), delta );
    for( final Iterator<IProfilPoint> points = p.getPoints().iterator(); points.hasNext(); )
    {
      final IProfilPoint point = points.next();
      final double breite = point.getValueFor( POINT_PROPERTY.BREITE );
      final double hoehe = point.getValueFor( POINT_PROPERTY.HOEHE );

      if( rangeO.contains( breite ) )
        point.setValueFor( POINT_PROPERTY.OBERKANTEWEHR, polyLineO.getYFor( breite ) );
      else
        point.setValueFor( POINT_PROPERTY.OBERKANTEWEHR, hoehe );

    }
    return true;
  }

  private double[] getWehrParameter( final String params )
  {
    final StringTokenizer sT = new StringTokenizer( params, " " );
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 1 )
    {
      return null;
    }
    // skip first parameter
    sT.nextToken();

    final double[] wp = new double[paramCount];
    for( int i = 0; i < paramCount; i++ )
    {
      wp[i] = Double.parseDouble( sT.nextToken() );
    }
    return wp;
  }

  private final String getWehrart( final String secLine )
  {
    final StringTokenizer sT = new StringTokenizer( secLine, " " );
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 0 )
    {
      return null;
    }
    final String wehrart = sT.nextToken().toUpperCase();
    if( wehrart.startsWith( "RUND" ) )
    {
      return IProfilConstants.WEHR_TYP_RUNDKRONIG;
    }
    if( wehrart.startsWith( "BREI" ) )
    {
      return IProfilConstants.WEHR_TYP_BREITKRONIG;
    }
    if( wehrart.startsWith( "SCHA" ) )
    {
      return IProfilConstants.WEHR_TYP_SCHARFKANTIG;
    }
    if( wehrart.startsWith( "BEIW" ) )
    {
      return IProfilConstants.WEHR_TYP_BEIWERT;
    }
    return null;
  }

  private void readBordVoll( final IProfil p, final PrfReader pr ) throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = p.getPoints();
    if( points.isEmpty() )
      return;
    final IDataBlock db = pr.getDataBlock( "BORDVOLL" );
    if( db == null )
      return;
    final int pCount = db.getCoordCount();
    IProfilPoint p1 = null;
    IProfilPoint p2 = null;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
    }
    if( pCount > 2 )
      m_logger.log( Level.INFO, "mehr als 2 Datensätze für Bordvollpunkte können an Station(" + p.getStation() + ") nicht ausgewertet werden" );
    // TODO: KIM in den Reparator verschieben
    // ---------------
    if( p1 == null )
    {
      p1 = points.getFirst();
      m_logger.log( Level.INFO, "Erzeuge Bordvollpunkt für Station(" + p.getStation() + ") an Position [" + Double.toString( p1.getValueFor( POINT_PROPERTY.BREITE ) ) + "]" );
    }
    if( p2 == null )
    {
      p2 = points.getLast();
      m_logger.log( Level.INFO, "Erzeuge Bordvollpunkt für Station(" + p.getStation() + ") an Position [" + Double.toString( p2.getValueFor( POINT_PROPERTY.BREITE ) ) + "]" );
    }
    // ----------------------------
    if( p1 != null )
      p.addDevider( p1, DEVIDER_TYP.BORDVOLL );
    if( p2 != null )
      p.addDevider( p2, DEVIDER_TYP.BORDVOLL );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSource#read(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public boolean read( final IProfil profil, final Reader reader )
  {

    final PrfReader prfReader = new PrfReader();
    try
    {
      prfReader.readFromReader( new BufferedReader( reader ) );
      readSource( prfReader, profil );
      return true;
    }
    catch( IOException e )
    {
      return false;
    }

  }

}
