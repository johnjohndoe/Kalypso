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

import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

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
  public ErrorFunctionFactory()
  {
  // TODO use transformationconstant in a better way, not as error-divisor !!
  // do not instantiate
  }

  public static IErrorFunktion createErrorFunktion( TreeMap measuredTS, AutoCalibration autocalibration )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();
    final FunctionMultiError result = new FunctionMultiError( measuredTS, startOptimize, endOptimize, 1d );
    final List errorFunktions = autocalibration.getOptParameter().getObjectiveFunction().getErrorFunktion();

    for( Iterator iter = errorFunktions.iterator(); iter.hasNext(); )
    {
      Object fXML = iter.next();
      final IErrorFunktion function;
      if( fXML instanceof VolumeError )
        function = createErrorFunction( (VolumeError)fXML, autocalibration, measuredTS );
      else if( fXML instanceof RootMeanSquareError )
        function = createErrorFunction( (RootMeanSquareError)fXML, autocalibration, measuredTS );
      else if( fXML instanceof RootMeanSquareErrorLowFlows )
        function = createErrorFunction( (RootMeanSquareErrorLowFlows)fXML, autocalibration, measuredTS );
      else if( fXML instanceof RootMeanSquareErrorPeakFlows )
        function = createErrorFunction( (RootMeanSquareErrorPeakFlows)fXML, autocalibration, measuredTS );
      else
        function = null;
      if( function != null )
        result.addFunction( function );
    }
    return result;
  }

  public static IErrorFunktion createErrorFunction( RootMeanSquareErrorLowFlows error, AutoCalibration autocalibration,
      TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionRMSEPeakAndLowFlow( calcedTS, startOptimize, endOptimize, error.getTransformationConstant(), 0d,
        error.getLowFlowLevel() );
  }

  public static IErrorFunktion createErrorFunction( RootMeanSquareErrorPeakFlows error,
      AutoCalibration autocalibration, TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionRMSEPeakAndLowFlow( calcedTS, startOptimize, endOptimize, error.getTransformationConstant(),
        error.getPeakFlowLevel(), FunctionRMSEPeakAndLowFlow.UNBOUND );
  }

  public static IErrorFunktion createErrorFunction( RootMeanSquareError error, AutoCalibration autocalibration,
      TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();
    return new FunctionRMSError( calcedTS, startOptimize, endOptimize, error.getTransformationConstant() );
  }

  public static IErrorFunktion createErrorFunction( VolumeError error, AutoCalibration autocalibration, TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionVolumeError( calcedTS, startOptimize, endOptimize, error.getTransformationConstant() );
  }
}