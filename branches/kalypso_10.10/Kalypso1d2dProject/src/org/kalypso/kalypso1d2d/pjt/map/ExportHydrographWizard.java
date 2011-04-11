package org.kalypso.kalypso1d2d.pjt.map;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.simulation.core.ISimulationConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

public class ExportHydrographWizard extends Wizard
{
  private Set<Integer> m_setExclusion = new HashSet<Integer>();

  private final static SimpleDateFormat m_dateFormat = new SimpleDateFormat( "dd.MM.yyyy" ); //$NON-NLS-1$

  private final static SimpleDateFormat m_timeFormat = new SimpleDateFormat( "HH:mm:SS.sss" ); //$NON-NLS-1$

  private static final int HEADER_COLS_COUNT = 8;

  private static final String COL_POS = "[x y]"; //$NON-NLS-1$

  ExportHydrographWizardPage m_exportHydrographWizardPage;

  private boolean m_boolValid = true;

  private IHydrographCollection m_hydrographs;

  private IHydrograph m_selectedHydrograph;

  private String m_textSep = "\t"; //$NON-NLS-1$

  private Map<XMLGregorianCalendar, String> m_mapDateStringsToPrint = null;

  private List<XMLGregorianCalendar> m_listDates = null;

  private IComponent[] m_componentsOrdered;

  private GM_Object m_geoPosition;

  private boolean m_boolHorizontalExport = true;

  private XMLGregorianCalendar m_actCalendarDate;

  private boolean m_boolPointSeparator = true;

  private Locale m_selectedLocale;

  private String m_outDefaultDir;

  public ExportHydrographWizard( final IHydrographCollection hydrographs, final IHydrograph selectedHydrograph )
  {
    setWindowTitle( "Hydrograph Export Wizard" ); //$NON-NLS-1$
    m_hydrographs = hydrographs;
    m_selectedHydrograph = selectedHydrograph;
    // exclude in normal case the depth
    m_setExclusion.add( 2 );
  }

  @Override
  public void addPages( )
  {
    IHydrograph lSelectedHydrograph = m_selectedHydrograph;
    if( lSelectedHydrograph == null )
    {

      if( m_hydrographs != null && m_hydrographs.size() > 0 )
      {
        lSelectedHydrograph = m_hydrographs.get( 0 );
      }
      else
      {
        m_boolValid = false;
      }
    }
    m_selectedHydrograph = lSelectedHydrograph;
    m_exportHydrographWizardPage = new ExportHydrographWizardPage( lSelectedHydrograph );
    addPage( m_exportHydrographWizardPage );
    m_exportHydrographWizardPage.setShell( this.getShell() );
    final String javaTmpDir = FrameworkProperties.getProperty( FileUtilities.JAVA_IO_TMPDIR );
    m_outDefaultDir = FrameworkProperties.getProperty( ISimulationConstants.SYSPROP_SIM_DIR, javaTmpDir );
    m_exportHydrographWizardPage.setDefaultOutDir( m_outDefaultDir );
  }

  @Override
  public boolean performFinish( )
  {
    if( m_boolValid )
    {
      final TupleResult tuples = m_selectedHydrograph.getObservation().getResult();

      final IComponent[] components = tuples.getComponents();

      final IComponent dateComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
      final IComponent waterlevelComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
      final IComponent depthComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DEPTH );
      final IComponent velocityComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY );
      final IComponent dischargeComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

      final IComponent waveHsigComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
      final IComponent wavePerComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER );
      final IComponent waveDirComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR );
      m_componentsOrdered = new IComponent[] { dateComp, waterlevelComp, depthComp, velocityComp, dischargeComp, waveHsigComp, wavePerComp, waveDirComp };
      
      if( m_exportHydrographWizardPage.getsSeparator() != null ){
        m_textSep = m_exportHydrographWizardPage.getsSeparator().getText().trim();
        if( m_textSep.charAt( 0 ) == '\\' ){
          if( m_textSep.charAt( 1 ) == 't' ){
            m_textSep = "\t"; //$NON-NLS-1$
          }
          else if( m_textSep.charAt( 1 ) == '0' ){
            m_textSep = "\0"; //$NON-NLS-1$
          }
          else if( m_textSep.charAt( 1 ) == 'r' ){
            m_textSep = "\r"; //$NON-NLS-1$
          }
          else if( m_textSep.charAt( 1 ) == 'n' ){
            m_textSep = "\n"; //$NON-NLS-1$
          }
        }
      }
            
      if( !m_exportHydrographWizardPage.getBtnCheckButton().getSelection() )
      {
        m_setExclusion.add( 1 );
      }
      if( !m_exportHydrographWizardPage.getBtnCheckButton_1().getSelection() )
      {
        m_setExclusion.add( 3 );
      }
      if( !m_exportHydrographWizardPage.getBtnCheckButton_2().getSelection() )
      {
        m_setExclusion.add( 4 );
      }
      if( !m_exportHydrographWizardPage.getBtnCheckButton_3().getSelection() )
      {
        m_setExclusion.add( 5 );
      }
      if( !m_exportHydrographWizardPage.getBtnCheckButton_4().getSelection() )
      {
        m_setExclusion.add( 6 );
      }
      if( !m_exportHydrographWizardPage.getBtnCheckButton_5().getSelection() )
      {
        m_setExclusion.add( 7 );
      }
      if( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() ){
        if( m_exportHydrographWizardPage.getBtnHorizontal().getSelection() ){
          m_boolHorizontalExport = true;
        }
        else{
          m_boolHorizontalExport = false;
        }
      }
      if( m_exportHydrographWizardPage.getBtnSeparatorButton_1().getSelection() ){
        m_boolPointSeparator = false;
      }
      else{
        m_boolPointSeparator = true;
      }
      
      runExport();

    }
    return true;
  }

  @SuppressWarnings("unchecked")
  public void runExport( )
  {
    String lStringExportDir = m_exportHydrographWizardPage.getsOutputDir().getText();
    if( lStringExportDir == null || lStringExportDir.trim() == "" ) //$NON-NLS-1$
    {
      
      lStringExportDir = m_outDefaultDir;
    }
    IHydrograph lSelectedHydrograph = m_selectedHydrograph;

    List<Feature> lListHydrographs = new ArrayList<Feature>();
    if( !m_exportHydrographWizardPage.getBtnCheckButtonOnlySelection().getSelection() )
    {
      lListHydrographs.addAll( m_hydrographs.getWrappedList() );
    }
    else
    {
      lListHydrographs.add( lSelectedHydrograph.getFeature() );
    }

    m_mapDateStringsToPrint = new HashMap<XMLGregorianCalendar, String>();
    m_listDates = new ArrayList<XMLGregorianCalendar>();
    OutputStream lOutStream = null;
    Formatter formatter = null;
    m_selectedLocale = Locale.US;
    if( !m_boolPointSeparator ){
      m_selectedLocale = Locale.GERMAN;
    }
    
    for( int lCountHydrographs = 0; lCountHydrographs < lListHydrographs.size(); ++lCountHydrographs )
    {

      IHydrograph selectedHydrograph = null;
      selectedHydrograph = (IHydrograph) (lListHydrographs.get( lCountHydrographs )).getAdapter( IHydrograph.class );
      m_geoPosition = selectedHydrograph.getLocation();
      Date lDateNow = new Date();
      final File stdOutFile = new File( lStringExportDir, /* "hydrograph_" + */selectedHydrograph.getName().replace( " ", "_" ) + "_" + lDateNow.getTime() + ".txt" ); //$NON-NLS-1$  //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

      if( m_exportHydrographWizardPage.getRadioMultipleFiles().getSelection() || lCountHydrographs == 0 )
      {
        try
        {
          lOutStream = new BufferedOutputStream( new FileOutputStream( stdOutFile ) );
        }
        catch( FileNotFoundException e1 )
        {
          e1.printStackTrace();
          return;
        }
        try
        {
          formatter = new Formatter( lOutStream, Charset.defaultCharset().name(), Locale.US );
        }
        catch( UnsupportedEncodingException e1 )
        {
          e1.printStackTrace();
          return;
        }
      }

      IObservation<TupleResult> observation = selectedHydrograph.getObservation();
      try
      {
        formatHeader( formatter, lCountHydrographs );
        writeObsLines( formatter, lCountHydrographs, observation );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      if( m_exportHydrographWizardPage.getRadioMultipleFiles().getSelection() )
      {

        if( formatter != null )
        {
          formatter.close();
        }
        if( lOutStream != null )
        {
          try
          {
            lOutStream.close();
          }
          catch( IOException e )
          {
            e.printStackTrace();
          }
        }
      }
    }

    if( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() )
    {
      if( m_boolHorizontalExport )
        printDataToFile( formatter );
      if( formatter != null )
      {
        formatter.close();
      }
      if( lOutStream != null )
      {
        try
        {
          lOutStream.close();
        }
        catch( IOException e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  /**
   * print the content of the filled map for horizontal export to the file
   */
  private void printDataToFile( final Formatter formatter )
  {
    formatter.format( "\n" ); //$NON-NLS-1$
    for( final XMLGregorianCalendar actDate : m_listDates )
    {
      formatter.format( "%s", m_mapDateStringsToPrint.get( actDate ) ); //$NON-NLS-1$
      formatter.format( "\n" ); //$NON-NLS-1$
    }
    formatter.flush();

  }

  /**
   * write all components to the given stream or append it to the according line in the map
   */
  private void writeObsLines( final Formatter formatter, final int lCountHydrographs, final IObservation<TupleResult> observation )
  {
    NumberFormat lNf = NumberFormat.getInstance( m_selectedLocale );
    lNf.setMinimumFractionDigits( 3 );
    lNf.setGroupingUsed( false );
    TupleResult obsResult = observation.getResult();
    String lPositionStr = String.format( "[%s %s]%s", lNf.format( m_geoPosition.getCentroid().getX() ), lNf.format( m_geoPosition.getCentroid().getY() ), m_textSep ); //$NON-NLS-1$
    for( int i = 0; i < obsResult.size(); ++i )
    {
      boolean lBoolPosSet = false;
      IRecord lResTuple = obsResult.get( i );
      int hydrographComponetsSize = HEADER_COLS_COUNT;

      if( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && m_boolHorizontalExport && lCountHydrographs > 0 )
      {
        m_setExclusion.add( 0 );
      }
      for( int j = 0; j < hydrographComponetsSize; ++j )
      {
        if( j == 0 )
        {
          m_actCalendarDate = (XMLGregorianCalendar) lResTuple.getValue( j );
          if( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && lCountHydrographs == 0 )
          {
            m_listDates.add( m_actCalendarDate );
          }
        }
        if( m_setExclusion.contains( j ) )
        {
          continue;
        }

        if( j == 0 )
        {
          String lStr = ""; //$NON-NLS-1$
          lStr += String.format( "%s%s%s%s", m_dateFormat.format( m_actCalendarDate.toGregorianCalendar().getTime() ), m_textSep, m_timeFormat.format( m_actCalendarDate.toGregorianCalendar().getTime() ), m_textSep ); //$NON-NLS-1$

          if( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && m_boolHorizontalExport )
          {
            String lStrOld = m_mapDateStringsToPrint.get( m_actCalendarDate );
            if( lStrOld == null )
            {
              lStrOld = ""; //$NON-NLS-1$
            }
            lStrOld += lStr;
            m_mapDateStringsToPrint.put( m_actCalendarDate, lStrOld );
          }
          else
          {
            formatter.format( "%s", lStr ); //$NON-NLS-1$
          }
        }
        else
        {
          String lStr = String.format( "%s%s", lNf.format( lResTuple.getValue( j ) ), m_textSep ); //$NON-NLS-1$

          if( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && m_boolHorizontalExport )
          {
            String lStrOld = m_mapDateStringsToPrint.get( m_actCalendarDate );
            if( lStrOld == null )
            { 
              lStrOld = ""; //$NON-NLS-1$
            }
            if( j > 0 && !lBoolPosSet )
            {
              lStrOld += lPositionStr;
              lBoolPosSet = true;
            }
            lStrOld += lStr;
            m_mapDateStringsToPrint.put( m_actCalendarDate, lStrOld );

          }
          else
          {
            if( j > 0 && !lBoolPosSet )
            {
              lStr = lPositionStr + lStr;
              lBoolPosSet = true;
            }
            formatter.format( "%s", lStr ); //$NON-NLS-1$
          }
        }
      }
      if( !(m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && m_boolHorizontalExport) )
      {
        formatter.format( "\n" ); //$NON-NLS-1$
      }
    }
    formatter.flush();
  }

  private void formatHeader( final Formatter formatter, final int pIntCounter )
  {
    if( pIntCounter == 0 || ( m_exportHydrographWizardPage.getRadioMultipleFiles().getSelection() ) )
    {
      formatter.format( "[DD.MM.YYYY]%s[HH:MM:SS.sss]%s", m_textSep, m_textSep ); //$NON-NLS-1$
    }
    if( pIntCounter > 0 && ( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && !m_boolHorizontalExport ) )
    {
      formatter.format( "\n" ); //$NON-NLS-1$
      formatter.flush();
      return;
    }
    for( int i = 1; i < HEADER_COLS_COUNT; ++i )
    {
      if( i == 1 )
      {
        formatter.format( "%s%s", COL_POS, m_textSep ); //$NON-NLS-1$
      }
      if( m_setExclusion.contains( i ) )
      {
        continue;
      }
      formatter.format( "%s%s", m_componentsOrdered[i].getDescription(), m_textSep ); //$NON-NLS-1$
    }
    if( m_exportHydrographWizardPage.getRadioMultipleFiles().getSelection() || ( m_exportHydrographWizardPage.getRadioSingleFile().getSelection() && !m_boolHorizontalExport ) )
    {
      formatter.format( "\n" ); //$NON-NLS-1$
    }
  }

}
