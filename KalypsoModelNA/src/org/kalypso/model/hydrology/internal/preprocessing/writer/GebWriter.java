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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;
import java.util.Locale;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Bodenschichtkorrektur;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Grundwasserabfluss;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.TimeseriesFileManager;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author doemming
 */
class GebWriter extends AbstractCoreFileWriter
{
  private final Catchment[] m_catchments;

  private final TimeseriesFileManager m_fileManager;

  private final NAControl m_metaControl;

  private final IDManager m_idManager;

  private StatusCollector m_log;

  public GebWriter( final Catchment[] catchments, final NAControl naControl, final TimeseriesFileManager fileManager, final IDManager idManager )
  {
    m_idManager = idManager;
    m_metaControl = naControl;
    m_catchments = catchments;
    m_fileManager = fileManager;
  }

  @Override
  protected IStatus writeContent( final PrintWriter writer ) throws NAPreprocessorException
  {
    m_log = new StatusCollector( ModelNA.PLUGIN_ID );

    for( final Catchment catchment : m_catchments )
      writeFeature( writer, catchment );

    return m_log.asMultiStatusOrOK( Messages.getString("GebWriter.0") ); //$NON-NLS-1$
  }

  private void writeFeature( final PrintWriter writer, final Catchment catchment ) throws NAPreprocessorException
  {
    // 0
    final int asciiID = m_idManager.getAsciiID( catchment );
    writer.printf( Locale.US, "%16d%7d\n", asciiID, 7 ); //$NON-NLS-1$

    // 1 (empty line)
    writer.append( "\n" ); //$NON-NLS-1$

    // 2
    // REMARK: formerly the area of the catchment geometry was written here.
    // But: the area of the catchment is not equal to the total area of it's hydrotopes, written to the we.hyd file.
    // And: the calculation does not use this area here since long again (at least since 2.1.8
    // double catchmentArea = catchment.getGeometry().getArea();
    // We now write '-9999' so it is clear that this number is never used.
    final double catchmentArea = -9999;
    final long area = Math.round( catchmentArea );
    writer.append( Long.toString( area ) );
    writer.append( "\n" ); //$NON-NLS-1$

    // 3
    // TODO: getNiederschlagEingabeDateiString is written twice here, check if it is correct
    final String niederschlagFile = m_fileManager.getNiederschlagEingabeDateiString( catchment );
    writer.printf( Locale.US, "%1$c %2$s %2$s %3$5.2f\n", m_metaControl.isUsePrecipitationForm() ? 's' : 'n', niederschlagFile, catchment.getFaktn() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    // 4-6
    final String temperaturFile = m_fileManager.getTemperaturEingabeDateiString( catchment );
    final String verdunstungFile = m_fileManager.getVerdunstungEingabeFilename( catchment );
    writer.printf( Locale.US, "%s %s\n", temperaturFile, verdunstungFile ); //$NON-NLS-1$

    // Zeitflächenfunktion
    final IObservation zft = catchment.getZft();
    if( zft == null )
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.0", catchment.getName() ); //$NON-NLS-1$
      throw new NAPreprocessorException( msg );
    }

    writer.append( "we_nat.zft\n" ); //$NON-NLS-1$
    writer.append( "we.hyd\n" ); //$NON-NLS-1$

    // 7
    // (snowtype,a15)(ftem,*)_(fver,*)
    writer.printf( "%-15s%s %s\n", catchment.getSnowtype(), catchment.getFtem(), catchment.getFver() ); //$NON-NLS-1$

    // 8
    writeBodenKorrektur( writer, catchment );

    // 10 (____(f_eva,f4.2)_(aint,f3.1)__(aigw,f6.2)____(fint,f4.2)____(ftra,f4.2))
    // JH: only "aigw" from gml. other parameters are not used by fortran program - dummys!
    final double aigw = catchment.getFaktorAigw() * catchment.getAigw();
    writer.printf( Locale.US, "%8.2f %3.1f %7.2f %7.2f %7.2f\n", 1.0, 0.0, aigw, 0.0, 0.0 ); //$NON-NLS-1$

    // 11 (retvs,*)_(retob,*)_(retint,*)_(retbas,*)_(retgw,*)_(retklu,*))
    // if correction factors of retention constants are choosen, retention constants correction

    // ATTENTION: the three faktors retob, retin and aigw have been (probably by mistake) applied twice to the real
    // value. We still do this here now in order to keep backwards compatibility.

    final double faktorRetobRetint = catchment.getFactorRetobRetint();

    final double retvs = catchment.getRetvs() * catchment.getFaktorRetvs();
    final double retob = catchment.getRetob() * catchment.getFaktorRetob() * faktorRetobRetint;
    final double retint = catchment.getRetint() * catchment.getFaktorRetint() * faktorRetobRetint;
    final double retbas = catchment.getRetbas() * catchment.getFaktorRetbas();
    final double retgw = catchment.getRetgw() * catchment.getFaktorRetgw();
    final double retklu = catchment.getRetklu() * catchment.getFaktorRetklu();

    writer.printf( Locale.US, "%f %f %f %f %f %f\n", retvs, retob, retint, retbas, retgw, retklu ); //$NON-NLS-1$

    // 12-14
    writeGrundwasserabfluss( catchment, writer );

    // 15
    writer.printf( Locale.US, "%f %f %f %f %f %f", catchment.getHgru(), catchment.getHgro(), catchment.getRtr(), catchment.getPors(), catchment.getGwsent(), catchment.getKlupor() ); //$NON-NLS-1$

    // tiefengrundwasser
    final Node izknNode = catchment.getIzknNode();

    if( izknNode == null )
      writer.append( " 0\n" ); //$NON-NLS-1$
    else
      writer.printf( Locale.US, " %4d\n", m_idManager.getAsciiID( izknNode ) ); //$NON-NLS-1$

    // KommentarZeile
    writer.append( "ende gebietsdatensatz\n" ); //$NON-NLS-1$//$NON-NLS-2$

  }

  private void writeGrundwasserabfluss( final Catchment catchment, final PrintWriter writer ) throws NAPreprocessorException
  {
    final Grundwasserabfluss[] grundwasserAbflussFeatures = catchment.getGrundwasserAbflussFeatures();
    writer.format( Locale.US, "%d\n", grundwasserAbflussFeatures.length ); //$NON-NLS-1$
    if( grundwasserAbflussFeatures.length == 0 )
      return;

    final double sumGwwi = catchment.getSumGwwi();
    final String catchmentName = catchment.getName();
    if( sumGwwi > 1.001 )
      throw new NAPreprocessorException( Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.84", catchmentName ) ); //$NON-NLS-1$

    if( sumGwwi < 0.999 )
    {
      final String formatString = Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.88" ); //$NON-NLS-1$
      m_log.add( IStatus.WARNING, formatString, null, catchmentName, sumGwwi * 100.0 );
    }

    writeLine13( catchment, sumGwwi, writer );
    writeLine14( catchment, sumGwwi, writer );
  }

  private void writeLine13( final Catchment catchment, final double sumGwwi, final PrintWriter writer ) throws NAPreprocessorException
  {
    final Grundwasserabfluss[] grundwasserAbflussFeatures = catchment.getGrundwasserAbflussFeatures();
    for( final Grundwasserabfluss gwa : grundwasserAbflussFeatures )
    {
      final Catchment linkedFE = gwa.getNgwzu();
      if( linkedFE == null )
        throw new NAPreprocessorException( Messages.getString( "org.kalypso.convert.namodel.manager.CatchmentManager.80", catchment.getName() ) ); //$NON-NLS-1$ 

      writer.format( Locale.US, "%d ", m_idManager.getAsciiID( linkedFE ) ); //$NON-NLS-1$
    }

    // Restanteil in virtuelles Teilgebiet außerhalb des Einzugsgebietes
    if( sumGwwi < 0.999 )
      writer.append( "0 " ); //$NON-NLS-1$

    writer.write( "\n" ); //$NON-NLS-1$
  }

  private void writeLine14( final Catchment catchment, final double sumGwwi, final PrintWriter writer )
  {
    final Grundwasserabfluss[] grundwasserAbflussFeatures = catchment.getGrundwasserAbflussFeatures();
    for( final Grundwasserabfluss gwa : grundwasserAbflussFeatures )
      writer.format( Locale.US, "%s ", gwa.getGwwi() ); //$NON-NLS-1$

    // Restanteil in virtuelles Teilgebiet außerhalb des Einzugsgebietes
    if( sumGwwi < 0.999 )
      writer.format( "%s ", 1 - sumGwwi ); //$NON-NLS-1$

    writer.append( "\n" ); //$NON-NLS-1$
  }

  private void writeBodenKorrektur( final PrintWriter writer, final Catchment catchment )
  {
    final Bodenschichtkorrektur[] bodenKorrekturFeatures = catchment.getBodenKorrekturFeatures();

    // Der Versiegelungsgrad vsg wird gesetzt, da er im Rechenkern aus der Hydrotopdatei Übernommen wird und somit in
    // der Gebietsdatei uninteressant ist.
    final Node overflowNode = catchment.getOverflowNode();
    final int overflowNodeID = overflowNode == null ? 0 : m_idManager.getAsciiID( overflowNode );
    final double bimax = 1.0;// JH: dummy for bimax, because it is not used in fortran!

    writer.format( Locale.US, "%5.3f %4d %9.1f %9.1f %4d %4.1f %4.1f\n", 1.0, bodenKorrekturFeatures.length, bimax, catchment.getBianf(), overflowNodeID, catchment.getTint(), catchment.getRintmx() ); //$NON-NLS-1$

    // 9
    for( final Bodenschichtkorrektur bodenKorrektur : bodenKorrekturFeatures )
    {
      // (cinh,*)_(cind,*)_(cex,*)_(bmax,*)_(banf,*)_(fko,*)_(retlay,*)_(evalay,*)
      final double cinh = bodenKorrektur.getCinh();
      final double cind = bodenKorrektur.getCind();
      final double cex = bodenKorrektur.getCex();
      final double bmax = bodenKorrektur.getBmax();
      final double banf = bodenKorrektur.getBanf();
      final double fko = bodenKorrektur.getFko();
      final double retlay = bodenKorrektur.getRetlay();
      // Dummy for "evalay", because the parameter is not used in fortran code
      final double evalay = 1.0;

      writer.format( Locale.US, "%s %s %s %s %s %s %s %.1f\n", cinh, cind, cex, bmax, banf, fko, retlay, evalay ); //$NON-NLS-1$
    }
  }
}