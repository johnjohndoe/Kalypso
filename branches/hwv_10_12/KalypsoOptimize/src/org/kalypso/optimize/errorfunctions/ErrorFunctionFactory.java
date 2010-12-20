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
package org.kalypso.optimize.errorfunctions;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.SortedMap;

import javax.xml.bind.JAXBElement;

import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.RootMeanSquareError;
import org.kalypso.optimizer.RootMeanSquareErrorLowFlows;
import org.kalypso.optimizer.RootMeanSquareErrorPeakFlows;
import org.kalypso.optimizer.VolumeError;

/**
 * factory class for creating error functions
 * 
 * @author doemming
 */
public class ErrorFunctionFactory
{
  private ErrorFunctionFactory( )
  {
    // do not instantiate
  }

  public static IErrorFunktion createErrorFunction( final SortedMap<Date, Double> measuredTS, final AutoCalibration autocalibration )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();
    final List<JAXBElement< ? >> errorFunktions = autocalibration.getOptParameter().getObjectiveFunction().getErrorFunktion();
    final List<IErrorFunktion> functions = new ArrayList<IErrorFunktion>();
    for( final JAXBElement< ? > element : errorFunktions )
    {
      final Object fXML = element.getValue();
      final IErrorFunktion function = toErrorFunktion( fXML, autocalibration, measuredTS );
      if( function != null )
        functions.add( function );
    }

    return new FunctionMultiError( measuredTS, startOptimize, endOptimize, functions.toArray( new IErrorFunktion[functions.size()] ) );
  }

  private static IErrorFunktion toErrorFunktion( final Object fXML, final AutoCalibration autocalibration, final SortedMap<Date, Double> measuredTS )
  {
    if( fXML instanceof VolumeError )
      return createErrorFunctionVolume( autocalibration, measuredTS );

    if( fXML instanceof RootMeanSquareError )
      return createErrorFunctionRMS( autocalibration, measuredTS );

    if( fXML instanceof RootMeanSquareErrorLowFlows )
      return createErrorFunction( (RootMeanSquareErrorLowFlows) fXML, autocalibration, measuredTS );

    if( fXML instanceof RootMeanSquareErrorPeakFlows )
      return createErrorFunction( (RootMeanSquareErrorPeakFlows) fXML, autocalibration, measuredTS );

    return null;
  } 

  public static IErrorFunktion createErrorFunction( final RootMeanSquareErrorLowFlows error, final AutoCalibration autocalibration,
      final SortedMap<Date, Double> calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionRMSEPeakAndLowFlow( calcedTS, startOptimize, endOptimize, 0d, error.getLowFlowLevel() );
  }

  public static IErrorFunktion createErrorFunction( final RootMeanSquareErrorPeakFlows error,
      final AutoCalibration autocalibration, final SortedMap<Date, Double> calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionRMSEPeakAndLowFlow( calcedTS, startOptimize, endOptimize, error.getPeakFlowLevel(),
        FunctionRMSEPeakAndLowFlow.UNBOUND );
  }

  public static IErrorFunktion createErrorFunctionRMS( final AutoCalibration autocalibration, final SortedMap<Date, Double> calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();
    return new FunctionRMSError( calcedTS, startOptimize, endOptimize );
  }

  public static IErrorFunktion createErrorFunctionVolume( final AutoCalibration autocalibration, final SortedMap<Date, Double> calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionVolumeError( calcedTS, startOptimize, endOptimize );
  }
}