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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.vfs2.FileObject;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;

/**
 * @author ig, <a href="mailto:ilya.gershovch@tu-harburg.de">ilya.gershovch@tu-harburg.de </a>
 */
public class Control1D2DConverterTelemac
{

  public static final String TELEMAC_CONTROL_FILE_HEADER = "%n" +
  		"/----------------------------------------------------------------------/%n" +
  		"/%n" +
  		"/                   STEERING FILE OF TELEMAC 2D%n" +
  		"/%n" +
//  		"/                       '" + m_projectName + "' TEST-CASE\n" +
//  		"              \n" +
//  		"/\n" +
//  		"/  PAS DE TEMPS DE 4 s :     86 s\n" +
//  		"/  PAS DE TEMPS DE 8 s :     44 s  5.6  04/10/2004\n" +
//  		"/\n" +
//  		"/  HP C3700 compilateur HP :  44 s  version 5.7 19/04/2007\n" +
//  		"/  HP C3700 compilateur Nag: 112 s  version 5.7 19/04/2007\n" +
//  		"/" +
//  		"/  HP C3700 compilateur HP            :  42 s  version 5.8 26/11/2007" +
//  		"/  HP C3700 compilateur Nag           : 107 s  version 5.8 26/12/2007" +
//  		"/  Dell 2.8 GHz Linux compilateur pgi :  32 s  version 5.8 19/12/2007" +
//  		"/" +
//  		"/  HP C3700 compilateur HP            :  43 s  version 5.9 16/10/2008" +
//  		"/  HP C3700 compilateur Nag           : 105 s  version 5.9 17/10/2008" +
//  		"/  Dell 2.8 GHz Linux compilateur pgi :  30 s  version 5.9 16/10/2008" +
//  		"/" +
//  		"/  HP C3700 compilateur HP              :  42 s  version 6.0 24/11/2009" +
//  		"/  HP C3700 compilateur Nag             : 102 s  version 6.0 27/11/2009" +
//  		"/  Dell 2.8 GHz Linux compilateur Intel :  23 s  version 6.0 26/11/2009" +
//  		"/" +
//  		"/----------------------------------------------------------------------/" +
//  		"/        INITIAL CONDITIONS IF NOT COMPUTATION CONTINUED" +
//  		"/        DO 10 I=1,NPOIN" +
//  		"                                                  " +
//  		"/          H%R(I) = MAX( 1.D0 , 23.77D0-ZF mod R(I) )" +
//  		"                             " +
//  		"/10      CONTINUE" +
  		"";
  
  /** Directory name for RMA-Kalypso result files (Output...) files */
  public static final String RESULT_DIR_NAME = "./"; //$NON-NLS-1$

  //  public static final String EXCLUSION_NUMBER = "-999.000000";    //$NON-NLS-1$

  public static final String COORD = "CART"; //$NON-NLS-1$

  public static final String INIT_DEF = "DEF"; //$NON-NLS-1$

  public static final String INIT_ZERO = "ZERO"; //$NON-NLS-1$

  public static final String INIT_PAR = "PAR"; //$NON-NLS-1$

  private static final String INIT_HOT = "HOT"; //$NON-NLS-1$

  private static final String SINGLE = " SINGLE "; //$NON-NLS-1$

  private static final String BOUNDARY_JSWAP = "JONswap"; //$NON-NLS-1$

  private static final String BOUNDARY_PM = "PM"; //$NON-NLS-1$

  private static final String BOUNDARY_GAUSS = "GAUSs"; //$NON-NLS-1$

  private static final String BOUNDARY_BIN = "BIN"; //$NON-NLS-1$

  /** Base filename name for RMA-Kalypso result files (Output...) files */
  public static final String RESULT_FILE_BASE = "Output"; //$NON-NLS-1$

  private final List<IBoundaryCondition> m_unitBoundaryConditions = new ArrayList<IBoundaryCondition>();

  private final IControlModel1D2D m_controlModel;

  private final ResultManager m_resultManager;

  private String m_strTimeFromToFormated = ""; //$NON-NLS-1$

  private String m_strTimeZeroFromToFormated = ""; //$NON-NLS-1$

  private String m_strTimeZeroFromFormated = ""; //$NON-NLS-1$

  private String m_strTimeFromZero = ""; //$NON-NLS-1$

  private String m_strTimeFrom = ""; //$NON-NLS-1$

  private String m_strTimeTo = ""; //$NON-NLS-1$

  private String m_strStepLength = ""; //$NON-NLS-1$

  private String m_strStepLengthUnit = "MIN"; //$NON-NLS-1$

  private String m_strStationary = ""; //$NON-NLS-1$

  private String m_strSeries = ""; //$NON-NLS-1$

  private final IGeoLog m_log;

  private Map<IFELine, Integer> m_mapContiLinesWithConditions;

  private Double m_version = 1.0;

//  private List<Date> m_listWritenDatesWind;

//  private RectifiedGridDomain m_gridDescriptor;

  private long m_intMilisecInMinute = 60000;

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private FileObject m_fileObjWorkingDir;

  private Date[] m_calculatedSteps;

  private Double m_doubleShiftY;

  private Double m_doubleShiftX;

  private String m_projectFileName;

  public Control1D2DConverterTelemac( final FileObject pFileObjWorkingDir, final String projectFileName, final IFEDiscretisationModel1d2d pDiscModel, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IGeoLog log, final ResultManager resultManager, final Map<IFELine, Integer> mapContiLineWithTelemacBoundaryToCondition,  double shiftX, double shiftY )
  {
    m_controlModel = controlModel;
    m_resultManager = resultManager;
    m_mapContiLinesWithConditions = mapContiLineWithTelemacBoundaryToCondition;
    m_discretisationModel = pDiscModel;
    m_projectFileName = projectFileName;
//    m_listWritenDatesWind = pListWrittenDates;
//    m_gridDescriptor = pWrittenGridDescriptor;
    m_log = log;
    m_fileObjWorkingDir = pFileObjWorkingDir;
    m_doubleShiftX = shiftX;
    m_doubleShiftY = shiftY;

    /* Initialize boundary conditions */
    final String calculationUnit = controlModel.getCalculationUnit().getId();
    for( final IFlowRelationship relationship : flowModel.getFlowRelationsShips() )
    {
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition boundaryCondition = (IBoundaryCondition) relationship;
        if( boundaryCondition.isMemberOf( calculationUnit ) )
          m_unitBoundaryConditions.add( boundaryCondition );
      }
    }
  }

  public void writeControlFile( final OutputStream outputStream ) throws IOException
  {
    Formatter formatter = null;
    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no local parameter for each format is needed any more .
      formatter = new Formatter( outputStream, Charset.defaultCharset().name(), Locale.US );
      writeInputControlFile( formatter );
      FormatterUtils.checkIoException( formatter );
    }
    finally
    {
      if( formatter != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        formatter.close();
      }
    }
  }

  /**
   * Writes the Control Data Block of the Telemac-Kalypso control file (INPUT)
   */

  public void writeInputControlFile( final Formatter formatter ) throws IOException
  {
    formatter.format( "PARALLEL PROCESSORS : %d%n", 1 ); //TODO: forward settings about processors from the UI
//    initTimeStrings();

//    formateCoordinatesShift();

    formateControlHeader( formatter );

//    formateMesh( formatter );

//    formateBottomData( formatter );

//    formateWaterLevelData( formatter );

//    formateWindData( formatter );

//    formateCurrentsData( formatter );

//    formateInitialBoundaries( formatter );

//    formateInitLine( formatter );

    formateSimulationOperationsSpec( formatter );

    formateSimulationOutputSpec( formatter );

    FormatterUtils.checkIoException( formatter );
    
    formatter.format( "&ETA           " );
  }

//  private void formateCoordinatesShift( ) throws IOException
//  {
//    Formatter lFormatter = getFormatter( ISimulation1D2DConstants.SIM_Telemac_COORD_SHIFT_FILE );
//    lFormatter.format( "Kalypso-Telemac Coordinates shift\n" ); //$NON-NLS-1$
//    lFormatter.format( "%s=%f\n%s=%f\nEND\n", ISimulation1D2DConstants.SIM_Telemac_COORD_SHIFT_X, m_doubleShiftX, ISimulation1D2DConstants.SIM_Telemac_COORD_SHIFT_Y, m_doubleShiftY ); //$NON-NLS-1$
//    lFormatter.close();
//  }
//
//  private void initTimeStrings( ) throws IOException
//  {
//    Date[] calculatedSteps = m_resultManager.findCalculatedSteps();
//    if( calculatedSteps != null && calculatedSteps.length > 1 )
//    {
//      calculatedSteps = TelemacAdditionalDataConverter.removeSteadyDates( calculatedSteps, null );
//    }
//    m_resultManager.setStepsToProcess( calculatedSteps, m_resultManager.getControlModel() );
//
//    m_calculatedSteps = calculatedSteps;
//    try
//    {
//
//      if( calculatedSteps != null && calculatedSteps.length > 1 )
//      {
//        long lLongStepLen = ((calculatedSteps[1].getTime() - calculatedSteps[0].getTime()) / m_intMilisecInMinute);
//        Date lDateZero = new Date( calculatedSteps[0].getTime() - lLongStepLen * m_intMilisecInMinute );
//        m_strTimeFromZero = TelemacDataConverterHelper.getTimeStringFormatedForTelemacInput( lDateZero );
//
//        m_strTimeFrom = TelemacDataConverterHelper.getTimeStringFormatedForTelemacInput( calculatedSteps[0] );
//        m_strTimeTo = TelemacDataConverterHelper.getTimeStringFormatedForTelemacInput( calculatedSteps[calculatedSteps.length - 1] );
//        m_strStepLength = "" + lLongStepLen; //$NON-NLS-1$
//      }
//      m_strStationary = m_controlModel.isUnsteadySelected() ? "NONSTATIONARY " : ""; //$NON-NLS-1$ //$NON-NLS-2$ //STATIONARY  
//      m_strSeries = (m_controlModel.isUnsteadySelected() ? "SERIES " : ""); //$NON-NLS-1$ //$NON-NLS-2$
//
//    }
//    catch( Exception e )
//    {
//      m_log.formatLog( 1, 1, e.getLocalizedMessage() );
//    }
//    m_strTimeFromToFormated = "" + (m_controlModel.isUnsteadySelected() ? m_strTimeFrom : "") + " " + m_strStepLength + " " + m_strStepLengthUnit + " " + //$NON-NLS-1$    //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
//        (m_controlModel.isUnsteadySelected() ? m_strTimeTo : ""); //$NON-NLS-1$  
//    m_strTimeZeroFromToFormated = "" + (m_controlModel.isUnsteadySelected() ? m_strTimeFromZero : "") + " " + m_strStepLength + " " + m_strStepLengthUnit + " " + //$NON-NLS-1$    //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
//        (m_controlModel.isUnsteadySelected() ? m_strTimeTo : ""); //$NON-NLS-1$  
//
//    m_strTimeZeroFromFormated = "" + (m_controlModel.isUnsteadySelected() ? m_strTimeFromZero : "") + " " + m_strStepLength + " " + m_strStepLengthUnit; //$NON-NLS-1$    //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
//  }

//  private String getSeriesFileNameFormated( final String pStrInitialFileName )
//  {
//    //    return pStrInitialFileName + (m_controlModel.isUnsteadySelected() ? ISimulation1D2DConstants.SIM_Telemac_TIME_SUFFIX + m_strStepLength : "") + ISimulation1D2DConstants.SIM_Telemac_DATA_FILE_EXT; //$NON-NLS-1$    
//    return pStrInitialFileName + ISimulation1D2DConstants.SIM_Telemac_DATA_FILE_EXT; //$NON-NLS-1$    
//  }

  /**
   * writes the header block with some defaults into the Telemac-Kalypso controlFile (INPUT)
   */
  @SuppressWarnings("deprecation")
  private void formateControlHeader( final Formatter formatter )
  {
    formatter.format( TELEMAC_CONTROL_FILE_HEADER );

  }


  /**
   * writes the water level information generated in RMA simulation before into the Telemac-Kalypso controlFile (INPUT)
   */
//  private void formateWaterLevelData( final Formatter formatter )
//  {
//    // format the water levels
//    formatter.format( "INPGRID WLEVEL UNSTRUCTURED %s %s\n" + //$NON-NLS-1$ 
//        "READINP WLEVEL 1. %s'%s' 1 0 FREE\n" + //$NON-NLS-1$
//        "$\n", m_strStationary, m_strTimeFromToFormated, m_strSeries, getSeriesFileNameFormated( ISimulation1D2DConstants.SIM_Telemac_WATER_LEVEL_DATA_FILE ) ); //$NON-NLS-1$
//  }

//  /**
//   * writes the wind data into the Telemac-Kalypso controlFile (INPUT)
//   */
//  private void formateWindData( final Formatter formatter )
//  {
//    // format the wind
//    try
//    {
//      if( m_controlModel.isConstantWindTelemac() )
//      {
//        formatter.format( "WIND %s\n$\n", //$NON-NLS-1$
//        m_controlModel.getConstantWindParTelemac() );
//      }
//      else
//      {
//        if( m_gridDescriptor == null )
//          return;
//        String lStrCRS = m_gridDescriptor.getCoordinateSystem();
//        String lStrStartTimeWind = TelemacDataConverterHelper.getTimeStringFormatedForTelemacInput( m_listWritenDatesWind.get( 0 ) );
//        String lStrEndTimeWind = TelemacDataConverterHelper.getTimeStringFormatedForTelemacInput( m_listWritenDatesWind.get( m_listWritenDatesWind.size() - 1 ) );
//        if( m_listWritenDatesWind.size() == 1 ){
//          formatter.format( "INPGRID WIND REG %d %d 0 %d %d %.1f %.1f %s %s\n", //$NON-NLS-1$ 
//              ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getX() - m_doubleShiftX)).intValue(), ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getY() - m_doubleShiftY)).intValue(), m_gridDescriptor.getNumColumns() - 1, m_gridDescriptor.getNumRows() - 1, Math.abs( m_gridDescriptor.getOffsetX( lStrCRS ) ), Math.abs( m_gridDescriptor.getOffsetY( lStrCRS ) ), m_strStationary, lStrStartTimeWind );
//        }
//        else{
//          String lStrTimeStepLen = "" + (m_listWritenDatesWind.get( 1 ).getTime() - m_listWritenDatesWind.get( 0 ).getTime()) / m_intMilisecInMinute; //$NON-NLS-1$  
//          formatter.format( "INPGRID WIND REG %d %d 0 %d %d %.1f %.1f %s %s %s %s %s\n", //$NON-NLS-1$ 
//              ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getX() - m_doubleShiftX)).intValue(), ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getY() - m_doubleShiftY)).intValue(), m_gridDescriptor.getNumColumns() - 1, m_gridDescriptor.getNumRows() - 1, Math.abs( m_gridDescriptor.getOffsetX( lStrCRS ) ), Math.abs( m_gridDescriptor.getOffsetY( lStrCRS ) ), m_strStationary, lStrStartTimeWind, lStrTimeStepLen, m_strStepLengthUnit, lStrEndTimeWind );
//        }
//        formatter.format( "READINP WIND 1. SERIES '%s' 3 0 FREE\n$\n", //$NON-NLS-1$ 
//        ISimulation1D2DConstants.SIM_Telemac_WIND_FILE + ISimulation1D2DConstants.SIM_Telemac_DATA_FILE_EXT );
//      }
//    }
//    catch( Exception e )
//    {
//      e.printStackTrace();
//    }
//  }

//  /**
//   * writes the currents information generated in RMA simulation before into the Telemac-Kalypso controlFile (INPUT)
//   */
//  private void formateCurrentsData( final Formatter formatter )
//  {
//    // //format the currents
//    formatter.format( "INPGRID CURRENT UNSTRUCTURED %s %s\n" + //$NON-NLS-1$
//        "READINP CURRENT 1. %s'%s' 1 0 FREE\n" + //$NON-NLS-1$
//        "$\n", //$NON-NLS-1$
//    m_strStationary, m_strTimeFromToFormated, m_strSeries, getSeriesFileNameFormated( ISimulation1D2DConstants.SIM_Telemac_CURRENT_DATA_FILE ) );
//
//  }

//  /**
//   * writes the initial condition into the Telemac-Kalypso controlFile (INPUT)
//   */
//  private void formateInitLine( final Formatter formatter )
//  {
//    // //format the currents
//    final Integer lInitVal = m_controlModel.getINITialValuesTelemac();
//    String lStrInitVal = INIT_DEF;
//    String lStrInitPAR = ""; //$NON-NLS-1$
//    switch( lInitVal )
//    {
//      case 0:
//        lStrInitVal = INIT_DEF;
//        break;
//      case 1:
//        lStrInitVal = INIT_ZERO;
//        break;
//      case 2:
//        lStrInitVal = INIT_PAR + m_controlModel.getINITialValuesParTelemac();
//        break;
//      case 3:
//        lStrInitVal = INIT_HOT + SINGLE + ISimulation1D2DConstants.SIM_Telemac_HOT_FILE;
//        break;
//      default:
//        lStrInitVal = INIT_DEF;
//    }
//    formatter.format( "INIT %s", lStrInitVal ); //$NON-NLS-1$
//
//    if( !"".equals( lStrInitPAR ) ) //$NON-NLS-1$
//    {
//      formatter.format( " %s", lStrInitPAR ); //$NON-NLS-1$
//    }
//    formatter.format( "\n" ); //$NON-NLS-1$
//  }

  /**
   * writes the Boundary Conditions based on continuity Lines Data Block and on this connected boundary conditions of
   * waves into the Telemac-Kalypso controlFile (INPUT)
   */
//  private void formateInitialBoundaries( final Formatter formatter ) throws IOException
//  {
//    final Set<IFELine> continuityLines = m_mapContiLinesWithConditions.keySet();
//
//    final Integer lIntBoundaryMethod = m_controlModel.getAlgBoundaryTelemac();
//    String lStrBoundaryMethod = BOUNDARY_JSWAP;
//    switch( lIntBoundaryMethod )
//    {
//      case 0:
//        lStrBoundaryMethod = BOUNDARY_JSWAP;
//        break;
//      case 1:
//        lStrBoundaryMethod = BOUNDARY_PM;
//        break;
//      case 2:
//        lStrBoundaryMethod = BOUNDARY_GAUSS;
//        break;
//      case 3:
//        lStrBoundaryMethod = BOUNDARY_BIN;
//      default:
//        lStrBoundaryMethod = BOUNDARY_JSWAP;
//        break;
//    }
//
//    for( final IFELine line : continuityLines )
//    {
//      int lIntContiLineId = m_mapContiLinesWithConditions.get( line );
//      // format the boundaries
//      formatter.format( "BOUN SHAPE %s PEAK DSPR POWER\n" //$NON-NLS-1$
//      , lStrBoundaryMethod );
//      formatBoundConds( formatter, lIntContiLineId );
//    }
//  }

  /**
   * writes the operations specification block for current simulation into the Telemac-Kalypso controlFile (INPUT)
   */
  private void formateSimulationOperationsSpec( final Formatter formatter )
  {
    formatter.format( "BOUNDARY CONDITIONS FILE          : %s%s%n", m_projectFileName, Gml2TelemacConv.BOUNDARY_NODES_FILE_EXTENTION );
    formatter.format( "GEOMETRY FILE                     : %s%s%s%n", Gml2TelemacConv.GEO_FILE_PREFIX, m_projectFileName, Gml2TelemacConv.SERAFIN_FILE_EXTENTION );
    formatter.format( "PREVIOUS COMPUTATION FILE         : %s%s%s%n", Gml2TelemacConv.GEO_FILE_PREFIX, m_projectFileName, Gml2TelemacConv.SERAFIN_FILE_EXTENTION );
//    formatter.format( "REFERENCE FILE                    : f2d_%s.slf", m_projectName );
    formatter.format( "RESULTS FILE                      : %s%s%s%n", Gml2TelemacConv.RESULT_FILE_PREFIX, m_projectFileName, Gml2TelemacConv.SERAFIN_FILE_EXTENTION );
    formatter.format( "LIQUID BOUNDARIES FILE            : %s%s%n", m_projectFileName, Gml2TelemacConv.BOUNDARY_CONDITIONS_FILE_EXTENTION );
    
//    String lOperationSpec = m_controlModel.getAdditionalSimParTelemac();
//    if( lOperationSpec != null )
//    {
//      formatter.format( "$\n" + //$NON-NLS-1$
//          "%s\n", //$NON-NLS-1$
//      lOperationSpec ); //$NON-NLS-1$
//
//    }
//    // TODO: what calculations do we exactly need, what can be default?
//    // setup the operations
//    // constant
//    else
//    {
//      formatter.format( "$\n" + //$NON-NLS-1$
//          "GEN3 KOMEN\n" + //$NON-NLS-1$
//          "BREAKING\n" + //$NON-NLS-1$
//          "FRICTION\n" + //$NON-NLS-1$
//          "TRIAD\n" + //$NON-NLS-1$
//          "$\n" ); //$NON-NLS-1$
//    }
  }

  /**
   * writes the output specification block for current simulation into the Telemac-Kalypso controlFile (INPUT)
   * 
   * //HSIG === significant wave height(m), WLEN === Average wave length (m) //RTP === peak period (in s) of the
   * variance density spectrum (relative frequency spectrum), //DIR === mean wave direction, WATLEV === water level (in
   * m), //VEL === current velocity (vector; in m/s), //FORCE === wave-induced force per unit surface area (vector; in
   * N/m2).
   */
  private void formateSimulationOutputSpec( final Formatter formatter )
  {
    String restartComent =  ( m_controlModel.getRestart() ? "/": "" ); 
    formatter.format( "/%n" +
"/%n" +
"TITLE                            = 'Demo2D'%n" +
"VARIABLES FOR GRAPHIC PRINTOUTS  = 'U,V,H,S,B'%n" +
"GRAPHIC PRINTOUT PERIOD          = 1%n" +
"LISTING PRINTOUT PERIOD          = 1%n" +
"MASS-BALANCE                     = YES%n" +
"TIME STEP                        = %d%n" +
"NUMBER OF TIME STEPS             = 25%n" + 
"INFORMATION ABOUT SOLVER         = YES%n" +
"/%n" +
"/----------------------------------------------%n" +
"/  INITIAL CONDITIONS%n" +
"/----------------------------------------------%n" +
"/%n" + 
"COMPUTATION CONTINUED            = " + ( m_controlModel.getRestart() ? "YES%n": "NO%n" ) + 
restartComent + "INITIAL CONDITIONS               = 'CONSTANT ELEVATION'%n" +
restartComent + "INITIAL ELEVATION                = %f%n" +
"/%n" +
"/----------------------------------------------%n" +
"/  BOUNDARY CONDITIONS %n" +
"/----------------------------------------------%n" +
"/%n" +
"/PRESCRIBED FLOWRATES             = 0.;0.035;0.070%n" +
"/PRESCRIBED ELEVATIONS            = 0.2852;0.;0.%n" +
"/VELOCITY PROFILES                = 2;2;2%n" +
"/%n" +
"/----------------------------------------------%n" +
"/  PHYSICAL PARAMETERS%n" +
"/----------------------------------------------%n" +
"/%n" +
"LAW OF BOTTOM FRICTION           = 5%n" +
"FRICTION COEFFICIENT             = 0.1%n" +
"TURBULENCE MODEL                 = 1%n" +
"VELOCITY DIFFUSIVITY             = 1.E-1%n" +
"/----------------------------------------------%n" +
"/  NUMERICAL PARAMETERS%n" + 
"/----------------------------------------------%n" +
"TYPE OF ADVECTION                 = 1;5%n" +
"SUPG OPTION                       = 2;2%n" +
"TREATMENT OF THE LINEAR SYSTEM    = 1%n" +       
"SOLVER                            = 1%n" +
"/SOLVER OPTION                     = 3%n" +  
"/IMPLICITATION FOR DEPTH           = 0.6%n" +
"/IMPLICITATION FOR VELOCITY        = 0.6%n" +     
"/MASS-LUMPING ON H                 = 1.%n" +
"/H CLIPPING                        = NO%n" +         
"/%n", 3600, m_controlModel.getELEV() ); //367.5
        
//    		"TITLE = 'TELEMAC 2D : RIVER CULM'  COMPUTATION CONTINUED :YES%n" +
//    		"INITIAL TIME SET TO ZERO : YES%n" +
//    		"TIME STEP = 8.     NUMBER OF TIME STEPS = 6750%n" +
//    		"GRAPHIC PRINTOUT PERIOD : 250%n" +
//    		"VARIABLES TO BE PRINTED : ''    LISTING PRINTOUT PERIOD =  250%n" +
//    		"VARIABLES FOR GRAPHIC PRINTOUTS : 'U,V,S,B,H'%n" +
//    		"LAW OF BOTTOM FRICTION = 3  FRICTION COEFFICIENT = 30.%n" +
//    		"VELOCITY DIFFUSIVITY = 2.        TURBULENCE MODEL : 1%n" +
//    		"SOLVER ACCURACY = 1.E-4      INFORMATION ABOUT SOLVER : OUI%n" +
//    		"/SOLVER : 7  SOLVER OPTION : 2   PRECONDITIONING : 2%n" +
//    		"IMPLICITATION FOR DEPTH = 1.         IMPLICITATION FOR VELOCITY = 1.%n" +
//    		"INITIAL CONDITIONS : 'CONSTANT ELEVATION' INITIAL ELEVATION : 50.%n" +
//    		"PRESCRIBED FLOWRATES : 28.32 ;  0.%n" +
//    		"PRESCRIBED ELEVATIONS : 0.   ; 23.77%n" +
//    		"MASS-BALANCE : YES  VALIDATION : YES%n" +
//    		"TYPE OF ADVECTION : 1;5%n" +
//    		"      SUPG OPTION : 1;0                MASS-LUMPING ON H : 1.%n" +
//    		"TIDAL FLATS : YES   OPTION FOR THE TREATMENT OF TIDAL FLATS : 1%n" +
//    		"CONTINUITY CORRECTION : NO%n" +
//    		"MATRIX STORAGE : 3     MATRIX-VECTOR PRODUCT : 2%n" +
//    		"TREATMENT OF THE LINEAR SYSTEM : 2 SOLVER : 1  PRECONDITIONING : 2  %n" );
    
//    // format the output of model
//    formatter.format( "$*******************MODEL OUTPUT************************\n" + //$NON-NLS-1$
//        "$\n" + //$NON-NLS-1$
//        "BLOCK 'COMPGRID' NOHEAD '%s.mat' XP YP HSIG TM01 DIR WATLEV WLEN FORCE VEL WIND %s \n" + //$NON-NLS-1$
//        "POINTS 'P_%s' FILE '%s.txt'\n" + //$NON-NLS-1$
//        "TABLE 'P_%s' HEAD '%s_out.tab' %s XP YP HSIG DIR TM01 WATLEV WIND VEL %s \n" + //$NON-NLS-1$   
//        "TEST 1,0\n" + //$NON-NLS-1$
//        "COMPUTE %s %s\n" + //$NON-NLS-1$ 
//        "STOP\n" + //$NON-NLS-1$
//        "$\n", //$NON-NLS-1$
//
//    ISimulation1D2DConstants.SIM_Telemac_TRIANGLE_FILE, m_controlModel.isUnsteadySelected() ? ("OUTPUT " + m_strTimeZeroFromFormated) : "", //$NON-NLS-1$   //$NON-NLS-2$
//
//    ISimulation1D2DConstants.SIM_Telemac_TRIANGLE_FILE, ISimulation1D2DConstants.SIM_Telemac_TRIANGLE_FILE, ISimulation1D2DConstants.SIM_Telemac_TRIANGLE_FILE, ISimulation1D2DConstants.SIM_Telemac_TRIANGLE_FILE, m_controlModel.isUnsteadySelected() ? "TIME " : "", //$NON-NLS-1$   //$NON-NLS-2$
//    m_controlModel.isUnsteadySelected() ? "OUTPUT " + m_strTimeZeroFromFormated : "", //$NON-NLS-1$   //$NON-NLS-2$
//    m_controlModel.isUnsteadySelected() ? "NONST" : "STATIONARY", //$NON-NLS-1$  //$NON-NLS-2$
//    m_strTimeZeroFromToFormated );

  }

//  /**
//   * Formats the lines for boundary condition ( Wave boundaries... ).
//   */
//  private void formatBoundConds( final Formatter pFormatter, final int pIntContiLineId ) throws IOException
//  {
//    for( final IBoundaryCondition boundaryCondition : m_unitBoundaryConditions )
//    {
//      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
//      {
//        final IObservation<TupleResult> obs = boundaryCondition.getObservation();
//        final TupleResult obsResult = obs.getResult();
//        final IComponent abscissaComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
//        IComponent ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
//
//        if( abscissaComponent != null && ordinateComponent != null )
//        {
//          IFELine lContiLineAct = null;
//          try
//          {
//            lContiLineAct = m_discretisationModel.findContinuityLine( boundaryCondition.getPosition(), 0.01 );
//          }
//          catch( Exception e )
//          {
//            continue;
//          }
//          if( m_mapContiLinesWithConditions.get( lContiLineAct ) != pIntContiLineId )
//          {
//            continue;
//          }
//          if( obsResult.size() == 0 || boundaryCondition.isAbsolute() )
//          {
//            pFormatter.format( "BOUN SIDE %d %s\n", pIntContiLineId, boundaryCondition.getStationaryCondition() ); //$NON-NLS-1$
//            return;
//          }
//
//          String lStrBoundFileNameAct = ISimulation1D2DConstants.Telemac_BOUNDARY_FILE_PREFIX + pIntContiLineId + ISimulation1D2DConstants.SIM_Telemac_DATA_FILE_EXT;
//          Formatter lFormatter = getFormatter( lStrBoundFileNameAct );
//          lFormatter.format( "TPAR\n" ); //$NON-NLS-1$
//
//          final IComponent lComponentTime = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
//          final IComponent lComponentHsig = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
//          final IComponent lComponentPer = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER );
//          final IComponent lComponentDir = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR );
//          final IComponent lComponentDD = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DD );
//          if( lComponentTime == null || lComponentHsig == null )
//            continue;
//
//          final TupleResultIndex tupleResultIndex = new TupleResultIndex( obsResult, lComponentTime );
//          final Iterator<IRecord> tupleIterator = tupleResultIndex.getIterator();
//          boolean lBoolWrittenBnd = false;
//          final int lIndexTime = obsResult.indexOfComponent( lComponentTime );
//          final int lIndexHsig = obsResult.indexOfComponent( lComponentHsig );
//          final int lIndexPer = obsResult.indexOfComponent( lComponentPer );
//          final int lIndexDir = obsResult.indexOfComponent( lComponentDir );
//          final int lIndexDD = obsResult.indexOfComponent( lComponentDD );
//          while( tupleIterator.hasNext() )
//          {
//            final IRecord record = tupleIterator.next();
//            final XMLGregorianCalendar lGregCalendar = (XMLGregorianCalendar) record.getValue( lIndexTime );
//
//            Date lDateAct = DateUtilities.toDate( lGregCalendar );
//            if( m_calculatedSteps[0].getTime() <= lDateAct.getTime() && m_calculatedSteps[m_calculatedSteps.length - 1].getTime() >= lDateAct.getTime() )
//            {
//              final String lStrTime = TelemacDataConverterHelper.getTimeStringFormatedForTelemacInput( lDateAct );
//
//              final BigDecimal lBDHsig = (BigDecimal) record.getValue( lIndexHsig );
//              final BigDecimal lBDPer = (BigDecimal) record.getValue( lIndexPer );
//              final BigDecimal lBDDir = (BigDecimal) record.getValue( lIndexDir );
//              final BigDecimal lBDDD = (BigDecimal) record.getValue( lIndexDD );
//              lFormatter.format( "%s %.2f %.2f %.2f %.2f\n", lStrTime, lBDHsig, lBDPer, lBDDir, lBDDD ); //$NON-NLS-1$
//              lBoolWrittenBnd = true;
//            }
//          }
//          if( lBoolWrittenBnd )
//          {
//            pFormatter.format( "BOUN SIDE %d CON FILE '%s'\n$\n", pIntContiLineId, lStrBoundFileNameAct ); //$NON-NLS-1$
//          }
//          else
//          {
//            pFormatter.format( "BOUN SIDE %d %s\n$\n", pIntContiLineId, boundaryCondition.getStationaryCondition() ); //$NON-NLS-1$
//          }
//
//          FormatterUtils.checkIoException( lFormatter );
//          lFormatter.close();
//        }
//      }
//
//      FormatterUtils.checkIoException( pFormatter );
//    }
//  }

  /**
   * 
   */
  protected Formatter getFormatter( final String pStrFileName ) throws IOException
  {
    Formatter lFormatter = null;
    {
      FileObject lFileDataAdditional = m_fileObjWorkingDir.resolveFile( pStrFileName );
      lFormatter = new Formatter( lFileDataAdditional.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
    }

    return lFormatter;
  }

}
