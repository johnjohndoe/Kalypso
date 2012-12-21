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
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.vfs2.FileObject;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * @author ig, <a href="mailto:ilya.gershovch@tu-harburg.de">ilya.gershovch@tu-harburg.de </a>
 */
public class Control1D2DConverterSWAN
{
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

  private final List<IBoundaryCondition> m_unitBoundaryConditions = new ArrayList<>();

  private final IControlModel1D2D m_controlModel;

  private final ResultManager m_resultManager;

  private String m_strTimeFromToFormated = ""; //$NON-NLS-1$

  private String m_strTimeZeroFromToFormated = ""; //$NON-NLS-1$

  private String m_strTimeZeroFromFormated = ""; //$NON-NLS-1$

  private String m_strTimeFromZero = ""; //$NON-NLS-1$

  private String m_strTimeFrom = ""; //$NON-NLS-1$

  private String m_strTimeTo = ""; //$NON-NLS-1$

  private String m_strStepLength = ""; //$NON-NLS-1$

  private final String m_strStepLengthUnit = "MIN"; //$NON-NLS-1$

  private String m_strStationary = ""; //$NON-NLS-1$

  private String m_strSeries = ""; //$NON-NLS-1$

  private final IGeoLog m_log;

  private final Map<IFELine, Integer> m_mapContiLinesWithConditions;

  private final Double m_version = 1.0;

  private final List<Date> m_listWritenDatesWind;

  private final RectifiedGridDomain m_gridDescriptor;

  private final long m_intMilisecInMinute = 60000;

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private final FileObject m_fileObjWorkingDir;

  private Date[] m_calculatedSteps;

  private final Double m_doubleShiftY;

  private final Double m_doubleShiftX;

  public Control1D2DConverterSWAN( final FileObject pFileObjWorkingDir, final IFEDiscretisationModel1d2d pDiscModel, final IControlModel1D2D controlModel, final IFlowRelationshipModel flowModel, final IGeoLog log, final ResultManager resultManager, final Map<IFELine, Integer> mapContiLineWithSWANBoundaryToCondition, final List<Date> pListWrittenDates, final RectifiedGridDomain pWrittenGridDescriptor, final double shiftX, final double shiftY )
  {
    m_controlModel = controlModel;
    m_resultManager = resultManager;
    m_mapContiLinesWithConditions = mapContiLineWithSWANBoundaryToCondition;
    m_discretisationModel = pDiscModel;
    m_listWritenDatesWind = pListWrittenDates;
    m_gridDescriptor = pWrittenGridDescriptor;
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
   * Writes the Control Data Block of the SWAN-Kalypso control file (INPUT)
   */

  public void writeInputControlFile( final Formatter formatter ) throws IOException
  {
    initTimeStrings();

    formateCoordinatesShift();

    formateControlHeader( formatter );

    formateMesh( formatter );

    formateBottomData( formatter );

    formateWaterLevelData( formatter );

    formateWindData( formatter );

    formateCurrentsData( formatter );

    formateInitialBoundaries( formatter );

    formateInitLine( formatter );

    formateSimulationOperationsSpec( formatter );

    formateSimulationOutputSpec( formatter );

    FormatterUtils.checkIoException( formatter );
  }

  private void formateCoordinatesShift( ) throws IOException
  {
    final Formatter lFormatter = getFormatter( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
    lFormatter.format( "Kalypso-SWAN Coordinates shift\n" ); //$NON-NLS-1$
    lFormatter.format( "%s=%f\n%s=%f\nEND\n", ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_X, m_doubleShiftX, ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_Y, m_doubleShiftY ); //$NON-NLS-1$
    lFormatter.close();
  }

  private void initTimeStrings( ) throws IOException
  {
    Date[] calculatedSteps = m_resultManager.findCalculatedSteps();
    if( calculatedSteps != null && calculatedSteps.length > 1 )
    {
      calculatedSteps = SWANAdditionalDataConverter.removeSteadyDates( calculatedSteps, null );
    }
    m_resultManager.setStepsToProcess( calculatedSteps );

    m_calculatedSteps = calculatedSteps;
    try
    {

      if( calculatedSteps != null && calculatedSteps.length > 1 )
      {
        final long lLongStepLen = ((calculatedSteps[1].getTime() - calculatedSteps[0].getTime()) / m_intMilisecInMinute);
        final Date lDateZero = new Date( calculatedSteps[0].getTime() - lLongStepLen * m_intMilisecInMinute );
        m_strTimeFromZero = SWANDataConverterHelper.getTimeStringFormatedForSWANInput( lDateZero );

        m_strTimeFrom = SWANDataConverterHelper.getTimeStringFormatedForSWANInput( calculatedSteps[0] );
        m_strTimeTo = SWANDataConverterHelper.getTimeStringFormatedForSWANInput( calculatedSteps[calculatedSteps.length - 1] );
        m_strStepLength = "" + lLongStepLen; //$NON-NLS-1$
      }
      m_strStationary = m_controlModel.isUnsteadySelected() ? "NONSTATIONARY " : ""; //$NON-NLS-1$ //$NON-NLS-2$ //STATIONARY
      m_strSeries = (m_controlModel.isUnsteadySelected() ? "SERIES " : ""); //$NON-NLS-1$ //$NON-NLS-2$

    }
    catch( final Exception e )
    {
      m_log.formatLog( 1, 1, e.getLocalizedMessage() );
    }
    m_strTimeFromToFormated = "" + (m_controlModel.isUnsteadySelected() ? m_strTimeFrom : "") + " " + m_strStepLength + " " + m_strStepLengthUnit + " " + //$NON-NLS-1$    //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        (m_controlModel.isUnsteadySelected() ? m_strTimeTo : ""); //$NON-NLS-1$
    m_strTimeZeroFromToFormated = "" + (m_controlModel.isUnsteadySelected() ? m_strTimeFromZero : "") + " " + m_strStepLength + " " + m_strStepLengthUnit + " " + //$NON-NLS-1$    //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        (m_controlModel.isUnsteadySelected() ? m_strTimeTo : ""); //$NON-NLS-1$

    m_strTimeZeroFromFormated = "" + (m_controlModel.isUnsteadySelected() ? m_strTimeFromZero : "") + " " + m_strStepLength + " " + m_strStepLengthUnit; //$NON-NLS-1$    //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  }

  private String getSeriesFileNameFormated( final String pStrInitialFileName )
  {
    //    return pStrInitialFileName + (m_controlModel.isUnsteadySelected() ? ISimulation1D2DConstants.SIM_SWAN_TIME_SUFFIX + m_strStepLength : "") + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT; //$NON-NLS-1$
    return pStrInitialFileName + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT; //$NON-NLS-1$
  }

  /**
   * writes the header block with some defaults into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateControlHeader( final Formatter formatter )
  {
    /* FILES DATA BLOCK */
    formatter.format( "$*************************HEADING************************\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "PROJ '%s' '%s'\n" + //$NON-NLS-1$
        "$'for %s  %s - %s\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "$ Field case: Kalypso Simulation \n" + //$NON-NLS-1$
        "$ Time of simulation: %s\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "$********************MODEL INPUT*************************\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "SET LEVEL %f\n" + //$NON-NLS-1$
        //        "SET NAUTICAL\n" + //$NON-NLS-1$
        "set maxerr=3\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "MODE %s TWOD\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "COORD %s\n" + //$NON-NLS-1$
        "$\n", //$NON-NLS-1$

    m_controlModel.getDescription(), m_version.toString(), m_controlModel.getId(), m_strTimeFrom, m_strTimeTo, ((new Date()).toGMTString()), // date
        // and time of actual simulation run
        0.0, // TODO: check if the default see level can be other
        m_strStationary, COORD ); // TODO: check if we have some different coordinates as Cartesian
  }

  /**
   * writes the definition of mesh file for SWAN into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateMesh( final Formatter formatter )
  {
    // format calculation grid
    // TODO: this settings can be also changed by user
    formatter.format( "CGRID UNSTRUCTURED CIRCLE 36 0.0521 1. 31\n" + //$NON-NLS-1$
        "READGRID UNSTRUCTURED triangle '%s'\n" + //$NON-NLS-1$
        "$\n", //$NON-NLS-1$
        ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE );
    formatter.format( "$internal shift in X:%f, internal shift in Y:%f\n$\n", m_doubleShiftX, m_doubleShiftY ); //$NON-NLS-1$
  }

  /**
   * writes the bottom informations into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateBottomData( final Formatter formatter )
  {
    // format the bottom
    formatter.format( "INPGRID BOTTOM UNSTRUCTURED EXC %s\n" + //$NON-NLS-1$
        "READINP BOTTOM -1. '%s.bot' 1 0 FREE\n" + //$NON-NLS-1$
        "$\n", //$NON-NLS-1$
        ISimulation1D2DConstants.SIM_SWAN_EXCLUSION_NUMBER, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE );

  }

  /**
   * writes the water level information generated in RMA simulation before into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateWaterLevelData( final Formatter formatter )
  {
    // format the water levels
    formatter.format( "INPGRID WLEVEL UNSTRUCTURED %s %s\n" + //$NON-NLS-1$
        "READINP WLEVEL 1. %s'%s' 1 0 FREE\n" + //$NON-NLS-1$
        "$\n", m_strStationary, m_strTimeFromToFormated, m_strSeries, getSeriesFileNameFormated( ISimulation1D2DConstants.SIM_SWAN_WATER_LEVEL_DATA_FILE ) ); //$NON-NLS-1$
  }

  /**
   * writes the wind data into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateWindData( final Formatter formatter )
  {
    // format the wind
    try
    {
      if( m_controlModel.isConstantWindSWAN() )
      {
        formatter.format( "WIND %s\n$\n", //$NON-NLS-1$
            m_controlModel.getConstantWindParSWAN() );
      }
      else
      {
        if( m_gridDescriptor == null )
          return;
        final String lStrCRS = m_gridDescriptor.getCoordinateSystem();
        final String lStrStartTimeWind = SWANDataConverterHelper.getTimeStringFormatedForSWANInput( m_listWritenDatesWind.get( 0 ) );
        final String lStrEndTimeWind = SWANDataConverterHelper.getTimeStringFormatedForSWANInput( m_listWritenDatesWind.get( m_listWritenDatesWind.size() - 1 ) );
        if( m_listWritenDatesWind.size() == 1 )
        {
          formatter.format( "INPGRID WIND REG %d %d 0 %d %d %.1f %.1f %s %s\n", //$NON-NLS-1$
              ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getX() - m_doubleShiftX)).intValue(), ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getY() - m_doubleShiftY)).intValue(), m_gridDescriptor.getNumColumns() - 1, m_gridDescriptor.getNumRows() - 1, Math.abs( m_gridDescriptor.getOffsetX( lStrCRS ) ), Math.abs( m_gridDescriptor.getOffsetY( lStrCRS ) ), m_strStationary, lStrStartTimeWind );
        }
        else
        {
          final String lStrTimeStepLen = "" + (m_listWritenDatesWind.get( 1 ).getTime() - m_listWritenDatesWind.get( 0 ).getTime()) / m_intMilisecInMinute; //$NON-NLS-1$
          formatter.format( "INPGRID WIND REG %d %d 0 %d %d %.1f %.1f %s %s %s %s %s\n", //$NON-NLS-1$
              ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getX() - m_doubleShiftX)).intValue(), ((Double) (m_gridDescriptor.getOrigin( lStrCRS ).getY() - m_doubleShiftY)).intValue(), m_gridDescriptor.getNumColumns() - 1, m_gridDescriptor.getNumRows() - 1, Math.abs( m_gridDescriptor.getOffsetX( lStrCRS ) ), Math.abs( m_gridDescriptor.getOffsetY( lStrCRS ) ), m_strStationary, lStrStartTimeWind, lStrTimeStepLen, m_strStepLengthUnit, lStrEndTimeWind );
        }
        formatter.format( "READINP WIND 1. SERIES '%s' 3 0 FREE\n$\n", //$NON-NLS-1$
            ISimulation1D2DConstants.SIM_SWAN_WIND_FILE + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * writes the currents information generated in RMA simulation before into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateCurrentsData( final Formatter formatter )
  {
    // //format the currents
    formatter.format( "INPGRID CURRENT UNSTRUCTURED %s %s\n" + //$NON-NLS-1$
        "READINP CURRENT 1. %s'%s' 1 0 FREE\n" + //$NON-NLS-1$
        "$\n", //$NON-NLS-1$
        m_strStationary, m_strTimeFromToFormated, m_strSeries, getSeriesFileNameFormated( ISimulation1D2DConstants.SIM_SWAN_CURRENT_DATA_FILE ) );

  }

  /**
   * writes the initial condition into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateInitLine( final Formatter formatter )
  {
    // //format the currents
    final Integer lInitVal = m_controlModel.getINITialValuesSWAN();
    String lStrInitVal = INIT_DEF;
    final String lStrInitPAR = ""; //$NON-NLS-1$
    switch( lInitVal )
    {
      case 0:
        lStrInitVal = INIT_DEF;
        break;
      case 1:
        lStrInitVal = INIT_ZERO;
        break;
      case 2:
        lStrInitVal = INIT_PAR + m_controlModel.getINITialValuesParSWAN();
        break;
      case 3:
        lStrInitVal = INIT_HOT + SINGLE + ISimulation1D2DConstants.SIM_SWAN_HOT_FILE;
        break;
      default:
        lStrInitVal = INIT_DEF;
    }
    formatter.format( "INIT %s", lStrInitVal ); //$NON-NLS-1$

    if( !"".equals( lStrInitPAR ) ) //$NON-NLS-1$
    {
      formatter.format( " %s", lStrInitPAR ); //$NON-NLS-1$
    }
    formatter.format( "\n" ); //$NON-NLS-1$
  }

  /**
   * writes the Boundary Conditions based on continuity Lines Data Block and on this connected boundary conditions of
   * waves into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateInitialBoundaries( final Formatter formatter ) throws IOException
  {
    final Set<IFELine> continuityLines = m_mapContiLinesWithConditions.keySet();

    final Integer lIntBoundaryMethod = m_controlModel.getAlgBoundarySWAN();
    String lStrBoundaryMethod = BOUNDARY_JSWAP;
    switch( lIntBoundaryMethod )
    {
      case 0:
        lStrBoundaryMethod = BOUNDARY_JSWAP;
        break;
      case 1:
        lStrBoundaryMethod = BOUNDARY_PM;
        break;
      case 2:
        lStrBoundaryMethod = BOUNDARY_GAUSS;
        break;
      case 3:
        lStrBoundaryMethod = BOUNDARY_BIN;
      default:
        lStrBoundaryMethod = BOUNDARY_JSWAP;
        break;
    }

    for( final IFELine line : continuityLines )
    {
      final int lIntContiLineId = m_mapContiLinesWithConditions.get( line );
      // format the boundaries
      formatter.format( "BOUN SHAPE %s PEAK DSPR POWER\n" //$NON-NLS-1$
      , lStrBoundaryMethod );
      formatBoundConds( formatter, lIntContiLineId );
    }
  }

  /**
   * writes the operations specification block for current simulation into the SWAN-Kalypso controlFile (INPUT)
   */
  private void formateSimulationOperationsSpec( final Formatter formatter )
  {
    final String lOperationSpec = m_controlModel.getAdditionalSimParSWAN();
    if( lOperationSpec != null )
    {
      formatter.format( "$\n" + //$NON-NLS-1$
          "%s\n", //$NON-NLS-1$
          lOperationSpec ); //$NON-NLS-1$

    }
    // TODO: what calculations do we exactly need, what can be default?
    // setup the operations
    // constant
    else
    {
      formatter.format( "$\n" + //$NON-NLS-1$
          "GEN3 KOMEN\n" + //$NON-NLS-1$
          "BREAKING\n" + //$NON-NLS-1$
          "FRICTION\n" + //$NON-NLS-1$
          "TRIAD\n" + //$NON-NLS-1$
          "$\n" ); //$NON-NLS-1$
    }
  }

  /**
   * writes the output specification block for current simulation into the SWAN-Kalypso controlFile (INPUT)
   *
   * //HSIG === significant wave height(m), WLEN === Average wave length (m) //RTP === peak period (in s) of the
   * variance density spectrum (relative frequency spectrum), //DIR === mean wave direction, WATLEV === water level (in
   * m), //VEL === current velocity (vector; in m/s), //FORCE === wave-induced force per unit surface area (vector; in
   * N/m2).
   */
  private void formateSimulationOutputSpec( final Formatter formatter )
  {
    // format the output of model
    formatter.format( "$*******************MODEL OUTPUT************************\n" + //$NON-NLS-1$
        "$\n" + //$NON-NLS-1$
        "BLOCK 'COMPGRID' NOHEAD '%s.mat' XP YP HSIG TM01 DIR WATLEV WLEN FORCE VEL WIND %s \n" + //$NON-NLS-1$
        "POINTS 'P_%s' FILE '%s.txt'\n" + //$NON-NLS-1$
        "TABLE 'P_%s' HEAD '%s_out.tab' %s XP YP HSIG DIR TM01 WATLEV WIND VEL %s \n" + //$NON-NLS-1$
        "TEST 1,0\n" + //$NON-NLS-1$
        "COMPUTE %s %s\n" + //$NON-NLS-1$
        "STOP\n" + //$NON-NLS-1$
        "$\n", //$NON-NLS-1$

    ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE, m_controlModel.isUnsteadySelected() ? ("OUTPUT " + m_strTimeZeroFromFormated) : "", //$NON-NLS-1$   //$NON-NLS-2$

    ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE, m_controlModel.isUnsteadySelected() ? "TIME " : "", //$NON-NLS-1$   //$NON-NLS-2$
        m_controlModel.isUnsteadySelected() ? "OUTPUT " + m_strTimeZeroFromFormated : "", //$NON-NLS-1$   //$NON-NLS-2$
        m_controlModel.isUnsteadySelected() ? "NONST" : "STATIONARY", //$NON-NLS-1$  //$NON-NLS-2$
        m_strTimeZeroFromToFormated );

  }

  /**
   * Formats the lines for boundary condition ( Wave boundaries... ).
   */
  private void formatBoundConds( final Formatter pFormatter, final int pIntContiLineId ) throws IOException
  {
    for( final IBoundaryCondition boundaryCondition : m_unitBoundaryConditions )
    {
      if( boundaryCondition.getTypeByLocation().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
      {
        final IObservation<TupleResult> obs = boundaryCondition.getObservation();
        final TupleResult obsResult = obs.getResult();
        final IComponent abscissaComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
        final IComponent ordinateComponent = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );

        if( abscissaComponent != null && ordinateComponent != null )
        {
          IFELine lContiLineAct = null;
          try
          {
            lContiLineAct = m_discretisationModel.findContinuityLine( boundaryCondition.getPosition(), 0.01 );
          }
          catch( final Exception e )
          {
            continue;
          }
          if( m_mapContiLinesWithConditions.get( lContiLineAct ) != pIntContiLineId )
          {
            continue;
          }
          if( obsResult.size() == 0 || boundaryCondition.isAbsolute() )
          {
            pFormatter.format( "BOUN SIDE %d %s\n", pIntContiLineId, boundaryCondition.getStationaryCondition() ); //$NON-NLS-1$
            return;
          }

          final String lStrBoundFileNameAct = ISimulation1D2DConstants.SWAN_BOUNDARY_FILE_PREFIX + pIntContiLineId + ISimulation1D2DConstants.SIM_SWAN_DATA_FILE_EXT;
          final Formatter lFormatter = getFormatter( lStrBoundFileNameAct );
          lFormatter.format( "TPAR\n" ); //$NON-NLS-1$

          final IComponent lComponentTime = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          final IComponent lComponentHsig = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
          final IComponent lComponentPer = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER );
          final IComponent lComponentDir = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR );
          final IComponent lComponentDD = TupleResultUtilities.findComponentById( obsResult, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DD );
          if( lComponentTime == null || lComponentHsig == null )
            continue;

          final TupleResultIndex tupleResultIndex = new TupleResultIndex( obsResult, lComponentTime );
          final Iterator<IRecord> tupleIterator = tupleResultIndex.getIterator();
          boolean lBoolWrittenBnd = false;
          final int lIndexTime = obsResult.indexOfComponent( lComponentTime );
          final int lIndexHsig = obsResult.indexOfComponent( lComponentHsig );
          final int lIndexPer = obsResult.indexOfComponent( lComponentPer );
          final int lIndexDir = obsResult.indexOfComponent( lComponentDir );
          final int lIndexDD = obsResult.indexOfComponent( lComponentDD );
          while( tupleIterator.hasNext() )
          {
            final IRecord record = tupleIterator.next();
            final XMLGregorianCalendar lGregCalendar = (XMLGregorianCalendar) record.getValue( lIndexTime );

            final Date lDateAct = DateUtilities.toDate( lGregCalendar );
            if( m_calculatedSteps[0].getTime() <= lDateAct.getTime() && m_calculatedSteps[m_calculatedSteps.length - 1].getTime() >= lDateAct.getTime() )
            {
              final String lStrTime = SWANDataConverterHelper.getTimeStringFormatedForSWANInput( lDateAct );

              final BigDecimal lBDHsig = (BigDecimal) record.getValue( lIndexHsig );
              final BigDecimal lBDPer = (BigDecimal) record.getValue( lIndexPer );
              final BigDecimal lBDDir = (BigDecimal) record.getValue( lIndexDir );
              final BigDecimal lBDDD = (BigDecimal) record.getValue( lIndexDD );
              lFormatter.format( "%s %.2f %.2f %.2f %.2f\n", lStrTime, lBDHsig, lBDPer, lBDDir, lBDDD ); //$NON-NLS-1$
              lBoolWrittenBnd = true;
            }
          }
          if( lBoolWrittenBnd )
          {
            pFormatter.format( "BOUN SIDE %d CON FILE '%s'\n$\n", pIntContiLineId, lStrBoundFileNameAct ); //$NON-NLS-1$
          }
          else
          {
            pFormatter.format( "BOUN SIDE %d %s\n$\n", pIntContiLineId, boundaryCondition.getStationaryCondition() ); //$NON-NLS-1$
          }

          FormatterUtils.checkIoException( lFormatter );
          lFormatter.close();
        }
      }

      FormatterUtils.checkIoException( pFormatter );
    }
  }

  /**
   *
   */
  protected Formatter getFormatter( final String pStrFileName ) throws IOException
  {
    Formatter lFormatter = null;
    {
      final FileObject lFileDataAdditional = m_fileObjWorkingDir.resolveFile( pStrFileName );
      lFormatter = new Formatter( lFileDataAdditional.getContent().getOutputStream(), Charset.defaultCharset().name(), Locale.US );
    }

    return lFormatter;
  }

}
