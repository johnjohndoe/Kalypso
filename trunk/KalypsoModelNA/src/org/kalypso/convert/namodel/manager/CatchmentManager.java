/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class CatchmentManager
{
  private final NAConfiguration m_conf;

  public static final String STD_TEMP_FILENAME = "std.tmp"; //$NON-NLS-1$

  public static final String STD_VERD_FILENAME = "std.ver"; //$NON-NLS-1$

  private static final HashMap<String, String> m_fileMap = new HashMap<String, String>();

  private final Logger m_logger;

  private final ASCIIHelper m_asciiHelper;

  public CatchmentManager( final NAConfiguration conf, final Logger logger )
  {
    m_asciiHelper = new ASCIIHelper( getClass().getResource( "resources/formats/WernerCatchment.txt" ) ); //$NON-NLS-1$
    m_conf = conf;
    m_logger = logger;
  }

  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace workspace ) throws Exception
  {
    final NaModell naModel = (NaModell) workspace.getRootFeature();

    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      if( asciiBuffer.isFeatureMarkedForWrite( catchment ) )
        writeFeature( asciiBuffer, workspace, catchment );
    }
  }

  private void writeFeature( final AsciiBuffer asciiBuffer, final GMLWorkspace workSpace, final Catchment catchment ) throws Exception
  {
    final StringBuffer catchmentBuffer = asciiBuffer.getCatchmentBuffer();

    // 0
    final IDManager idManager = m_conf.getIdManager();
    final int asciiID = idManager.getAsciiID( catchment );
    catchmentBuffer.append( String.format( "           %5d      7\n", asciiID ) ); //$NON-NLS-1$

    // 1-2
    for( int i = 1; i <= 2; i++ )
      catchmentBuffer.append( m_asciiHelper.toAscii( catchment, i ) + "\n" ); //$NON-NLS-1$

    // 3
    if( m_conf.getMetaControl().isUsePrecipitationForm() )
      catchmentBuffer.append( "s " ); //$NON-NLS-1$
    else
      catchmentBuffer.append( "n " ); //$NON-NLS-1$

    catchmentBuffer.append( getNiederschlagEingabeDateiString( catchment, m_conf ) );
    catchmentBuffer.append( " " + getNiederschlagEingabeDateiString( catchment, m_conf ) ); //$NON-NLS-1$
    catchmentBuffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "faktn" ), "f5.2" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    // 4-6
    catchmentBuffer.append( getTemperaturEingabeDateiString( catchment, m_conf ) );
    catchmentBuffer.append( " " ); //$NON-NLS-1$
    catchmentBuffer.append( getVerdunstungEingabeDateiString( catchment, m_conf ) );
    catchmentBuffer.append( "\n" ); //$NON-NLS-1$

    // Zeitflächenfunktion
    final Object zftProp = catchment.getProperty( NaModelConstants.CATCHMENT_PROP_ZFT );
    if( zftProp instanceof IObservation )
    {
      catchmentBuffer.append( "we_nat.zft\n" ); //$NON-NLS-1$
      writeZML( (IObservation) zftProp, asciiID, asciiBuffer.getZFTBuffer() );
    }
    else
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.0", asciiID ); //$NON-NLS-1$
      m_logger.log( Level.WARNING, msg );

      catchmentBuffer.append( "we999.zfl\n" ); //$NON-NLS-1$

      // BUG: this can never work, as the we999 file is not available
      // TODO: copy the we999 into the inp.dat folder or stop calculation!
    }
    catchmentBuffer.append( "we.hyd\n" ); //$NON-NLS-1$

    // 7
    catchmentBuffer.append( m_asciiHelper.toAscii( catchment, 7 ) + "\n" ); //$NON-NLS-1$

    // 8
    final List< ? > list = (List< ? >) catchment.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );

    // Der Versiegelungsgrad vsg wird gesetzt, da er im Rechenkern aus der Hydrotopdatei übernommen wird und somit in
    // der Gebietsdatei uninteressant ist.
    catchmentBuffer.append( String.format( "1.000%5d", list.size() ) ); //$NON-NLS-1$ //$NON-NLS-2$
    catchmentBuffer.append( "       1.0" ); // JH: dummy for bimax, because it is not used in fortran! //$NON-NLS-1$ //$NON-NLS-2$
    catchmentBuffer.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "bianf" ), "f5.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final IRelationType rt = (IRelationType) catchment.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_IZKN_VERS );
    final Feature nodeFeVers = workSpace.resolveLink( catchment, rt );
    if( nodeFeVers == null )
      catchmentBuffer.append( "    0" ); //$NON-NLS-1$
    else
      catchmentBuffer.append( FortranFormatHelper.printf( Integer.toString( idManager.getAsciiID( nodeFeVers ) ), "i5" ) ); //$NON-NLS-1$

    catchmentBuffer.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "tint" ), "f5.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    catchmentBuffer.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "rintmx" ), "f5.1" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    // 9 (cinh,*)_(cind,*)_(cex,*)_(bmax,*)_(banf,*)_(fko,*)_(retlay,*)
    // JH: + dummy for "evalay", because the parameter is not used in fortran code
    final Iterator< ? > iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature fe = (Feature) iter.next();
      catchmentBuffer.append( m_asciiHelper.toAscii( fe, 9 ) + " 1.0" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    // 10 (____(f_eva,f4.2)_(aint,f3.1)__(aigw,f6.2)____(fint,f4.2)____(ftra,f4.2))
    // JH: only "aigw" from gml. other parameters are not used by fortran program - dummys!
    final double aigw = catchment.getFaktorAigw() * catchment.getAigw();
    catchmentBuffer.append( String.format( "1.00 0.0 %6.2f 0.00 0.00\n", aigw ) ); //$NON-NLS-1$

    // 11 (retvs,*)_(retob,*)_(retint,*)_(retbas,*)_(retgw,*)_(retklu,*))
    // if correction factors of retention constants are choosen, retention constants correction
    final Double faktorRetvs = FeatureHelper.getAsDouble( catchment, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETVS, 1 );
    final double faktorRetob = catchment.getFaktorRetob();
    final double faktorRetint = catchment.getFaktorRetint();
    final Double faktorRetbas = FeatureHelper.getAsDouble( catchment, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETBAS, 1 );
    final Double faktorRetgw = FeatureHelper.getAsDouble( catchment, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETGW, 1 );
    final Double faktorRetklu = FeatureHelper.getAsDouble( catchment, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETKLU, 1 );
    final double retvs = faktorRetvs.doubleValue() * ((Double) catchment.getProperty( NaModelConstants.CATCHMENT_PROP_RETVS )).doubleValue();

    // ATTENTION: the three faktors retob, retin and aigw have been (probably by mistake) applied twice to the real
    // value. We still do this here now in order to keep backwards compatibility.
    final double retob = faktorRetob * catchment.getRetob();
    final double retint = faktorRetint * catchment.getRetint();

    final double retbas = faktorRetbas.doubleValue() * ((Double) catchment.getProperty( NaModelConstants.CATCHMENT_PROP_RETBAS )).doubleValue();
    final double retgw = faktorRetgw.doubleValue() * ((Double) catchment.getProperty( NaModelConstants.CATCHMENT_PROP_RETGW )).doubleValue();
    final double retklu = faktorRetklu.doubleValue() * ((Double) catchment.getProperty( NaModelConstants.CATCHMENT_PROP_RETKLU )).doubleValue();

    catchmentBuffer.append( FortranFormatHelper.printf( retvs, "*" ) + " " + FortranFormatHelper.printf( retob, "*" ) + " " + FortranFormatHelper.printf( retint, "*" ) + " " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        + FortranFormatHelper.printf( retbas, "*" ) + " " + FortranFormatHelper.printf( retgw, "*" ) + " " + FortranFormatHelper.printf( retklu, "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

    // 12-14
    final List< ? > gwList = (List< ? >) catchment.getProperty( NaModelConstants.GRUNDWASSERABFLUSS_MEMBER );
    catchmentBuffer.append( FortranFormatHelper.printf( Integer.toString( gwList.size() ), "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final StringBuffer line13 = new StringBuffer();
    final StringBuffer line14 = new StringBuffer();
    double sumGwwi = 0.0;
    for( final Object name : gwList )
    {
      final Feature fe = (Feature) name;
      final IRelationType rt2 = (IRelationType) fe.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_NGWZU );
      final Feature linkedFE = workSpace.resolveLink( fe, rt2 );

      if( linkedFE == null )
        throw new Exception( Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.80", FeatureHelper.getAsString( fe, "ngwzu" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
      line13.append( Integer.toString( idManager.getAsciiID( linkedFE ) ).trim() + " " ); //$NON-NLS-1$
      line14.append( m_asciiHelper.toAscii( fe, 14 ) + " " ); //$NON-NLS-1$
      sumGwwi += ((Double) fe.getProperty( NaModelConstants.CATCHMENT_PROP_GWWI )).doubleValue();
    }

    if( sumGwwi > 1.001 )
      throw new Exception( Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.84", catchment.getName() ) //$NON-NLS-1$
          + ", AsciiID: " + asciiID ); //$NON-NLS-1$
    if( sumGwwi < 0.999 )
    {
      // Restanteil in virtuelles Teilgebiet außerhalb des Einzugsgebietes
      final double delta = 1 - sumGwwi;
      line13.append( "0 " ); //$NON-NLS-1$
      line14.append( delta + " " ); //$NON-NLS-1$
      Logger.getAnonymousLogger().log( Level.WARNING, String.format( Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.88" ), catchment.getName(), Integer.toString( asciiID ), sumGwwi * 100.0 ) ); //$NON-NLS-1$
      Logger.getAnonymousLogger().log( Level.WARNING, String.format( Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.92" ), delta * 100.0 ) ); //$NON-NLS-1$
    }

    if( gwList.size() > 0 )
    {
      catchmentBuffer.append( line13 + "\n" ); //$NON-NLS-1$
      catchmentBuffer.append( line14 + "\n" ); //$NON-NLS-1$
    }

    // 15
    catchmentBuffer.append( FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "hgru" ), "*" ) ); //$NON-NLS-1$//$NON-NLS-2$
    catchmentBuffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "hgro" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    catchmentBuffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "rtr" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    catchmentBuffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "pors" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    catchmentBuffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "gwsent" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    catchmentBuffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( catchment, "klupor" ), "*" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    // tiefengrundwasser
    final IRelationType rt1 = (IRelationType) catchment.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_IZKN );
    final Feature nodeFeGW = workSpace.resolveLink( catchment, rt1 );

    if( nodeFeGW == null )
      catchmentBuffer.append( " 0\n" ); //$NON-NLS-1$
    else
      catchmentBuffer.append( FortranFormatHelper.printf( Integer.toString( idManager.getAsciiID( nodeFeGW ) ), "i5" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    // KommentarZeile
    catchmentBuffer.append( "ende gebietsdatensatz" + "\n" ); //$NON-NLS-1$//$NON-NLS-2$

  }

  /**
   * @param observation
   * @param asciiID
   * @param zftBuffer
   * @throws SensorException
   */
  private void writeZML( final IObservation observation, final int asciiID, final StringBuffer zftBuffer ) throws SensorException
  {
    zftBuffer.append( FortranFormatHelper.printf( asciiID, "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    final IAxis[] axisList = observation.getAxisList();
    final IAxis hoursAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_HOURS );
    final IAxis normAreaAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_NORM );
    final ITupleModel values = observation.getValues( null );
    final int count = values.getCount();
    final double t0 = ((Double) values.getElement( 0, hoursAxis )).doubleValue();
    final double t1 = ((Double) values.getElement( 1, hoursAxis )).doubleValue();
    final double dt = t1 - t0;
    zftBuffer.append( FortranFormatHelper.printf( count, "*" ) + " " + FortranFormatHelper.printf( dt, "*" ) + " 2\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    for( int row = 0; row < count; row++ )
    {
      final Double hoursValue = (Double) values.getElement( row, hoursAxis );
      final Double normAreaValue = (Double) values.getElement( row, normAreaAxis );
      zftBuffer.append( FortranFormatHelper.printf( hoursValue, "*" ) + " " + FortranFormatHelper.printf( normAreaValue, "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
  }

  public static String getEingabeDateiString( final Feature feature, final NAConfiguration conf, final String propName, final String axisType )
  {
    final String key;
    if( propName.equals( "synthZR" ) ) //$NON-NLS-1$
    {
      key = (String) feature.getProperty( new QName( NaModelConstants.NS_NAMODELL, propName ) );
    }
    else
    {
      final TimeseriesLinkType link = (TimeseriesLinkType) feature.getProperty( new QName( NaModelConstants.NS_NAMODELL, propName ) );
      key = propName + link.getHref();
    }
    if( !m_fileMap.containsKey( key ) )
    {
      final int asciiID = conf.getIdManager().getAsciiID( feature );
      final String name = "C_" + Integer.toString( asciiID ).trim() + "." + axisType; //$NON-NLS-1$//$NON-NLS-2$
      m_fileMap.put( key, name );
    }
    return m_fileMap.get( key );
  }

  public static String getNiederschlagEingabeDateiString( final Feature feature, final NAConfiguration conf )
  {
    final NAControl metaControl = conf.getMetaControl();
    if( metaControl.isUsePrecipitationForm() )
      return getEingabeDateiString( feature, conf, "synthZR", ITimeseriesConstants.TYPE_RAINFALL ); //$NON-NLS-1$

    return getEingabeDateiString( feature, conf, "niederschlagZR", ITimeseriesConstants.TYPE_RAINFALL ); //$NON-NLS-1$

  }

  public static String getTemperaturEingabeDateiString( final Feature feature, final NAConfiguration conf )
  {
    if( feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_TEMPERATUR ) != null )
      return getEingabeDateiString( feature, conf, "temperaturZR", ITimeseriesConstants.TYPE_TEMPERATURE ); //$NON-NLS-1$
    return STD_TEMP_FILENAME;

  }

  /**
   * @param feature
   * @param dir
   */
  public static File getTemperaturEingabeDatei( final Feature feature, final File dir, final NAConfiguration conf )
  {
    final String name = getTemperaturEingabeDateiString( feature, conf );
    return new File( dir, name );
  }

  /**
   * @param feature
   */
  public static File getNiederschlagEingabeDatei( final Feature feature, final File dir, final NAConfiguration conf )
  {
    return new File( dir, getNiederschlagEingabeDateiString( feature, conf ) );
  }

  public static File getVerdunstungEingabeDatei( final Feature feature, final File dir, final NAConfiguration conf )
  {
    final String name = getVerdunstungEingabeDateiString( feature, conf );
    return new File( dir, name );
  }

  private static String getVerdunstungEingabeDateiString( final Feature feature, final NAConfiguration conf )
  {
    if( feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_VERDUNSTUNG ) != null )
      return getEingabeDateiString( feature, conf, "verdunstungZR", ITimeseriesConstants.TYPE_EVAPORATION ); //$NON-NLS-1$
    return STD_VERD_FILENAME;

    // int asciiID = conf.getIdManager().getAsciiID( feature );
    // if( feature.getProperty( "verdunstungZR" ) != null )
    // return "C_" + Integer.toString( asciiID ).trim() + ".ver";
  }
}