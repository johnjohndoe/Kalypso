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
  // do not instantiate
  }

  public static IErrorFunktion createErrorFunktion( TreeMap measuredTS,
      AutoCalibration autocalibration )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();
    final FunctionMultiError result = new FunctionMultiError( measuredTS, startOptimize,
        endOptimize, 1d );
    final List errorFunktions = autocalibration.getOptParameter().getObjectiveFunction()
        .getErrorFunktion();

    for( Iterator iter = errorFunktions.iterator(); iter.hasNext(); )
    {
      Object fXML = iter.next();
      final IErrorFunktion function;
      if( fXML instanceof VolumeError )
        function = createErrorFunction( (VolumeError)fXML, autocalibration, measuredTS );
      else if( fXML instanceof RootMeanSquareError )
        function = createErrorFunction( (RootMeanSquareError)fXML, autocalibration, measuredTS );
      else if( fXML instanceof RootMeanSquareErrorLowFlows )
        function = createErrorFunction( (RootMeanSquareErrorLowFlows)fXML, autocalibration,
            measuredTS );
      else if( fXML instanceof RootMeanSquareErrorPeakFlows )
        function = createErrorFunction( (RootMeanSquareErrorPeakFlows)fXML, autocalibration,
            measuredTS );
      else
        function = null;
      if( function != null )
        result.addFunction( function );
    }
    return result;
  }

  public static IErrorFunktion createErrorFunction( RootMeanSquareErrorLowFlows error,
      AutoCalibration autocalibration, TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionRMSEPeakAndLowFlow( calcedTS, startOptimize, endOptimize, error
        .getTransformationConstant(), 0d, error.getLowFlowLevel() );
  }

  public static IErrorFunktion createErrorFunction( RootMeanSquareErrorPeakFlows error,
      AutoCalibration autocalibration, TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionRMSEPeakAndLowFlow( calcedTS, startOptimize, endOptimize, error
        .getTransformationConstant(), error.getPeakFlowLevel(), FunctionRMSEPeakAndLowFlow.UNBOUND );
  }

  public static IErrorFunktion createErrorFunction( RootMeanSquareError error,
      AutoCalibration autocalibration, TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();
    return new FunctionRMSError( calcedTS, startOptimize, endOptimize, error
        .getTransformationConstant() );
  }

  public static IErrorFunktion createErrorFunction( VolumeError error,
      AutoCalibration autocalibration, TreeMap calcedTS )
  {
    final Date startOptimize = autocalibration.getPegel().getStartDate().getTime();
    final Date endOptimize = autocalibration.getPegel().getEndDate().getTime();

    return new FunctionVolumeError( calcedTS, startOptimize, endOptimize, error
        .getTransformationConstant() );
  }
}