package org.kalypso.model.wspm.sobek.result.processing.worker;

import nl.wldelft.fews.pi.HeaderComplexType;
import nl.wldelft.fews.pi.TimeStepComplexType;
import nl.wldelft.fews.pi.TimeStepUnitEnumStringType;

import org.apache.commons.lang.NotImplementedException;

public class ResultHelper
{

  public static int getIntervallAsSeconds( final HeaderComplexType header )
  {
    final TimeStepComplexType timeStep = header.getTimeStep();
    final int divider = timeStep.getDivider().intValue();
    final int multiplier = timeStep.getMultiplier().intValue();

    if( divider != 1 )
      throw new NotImplementedException();

    final TimeStepUnitEnumStringType eUnit = timeStep.getUnit();

    if( TimeStepUnitEnumStringType.HOUR.equals( eUnit ) )
    {
      final int minutes = multiplier * 60;
      final int seconds = minutes * 60;

      return seconds;
    }
    else if( TimeStepUnitEnumStringType.MINUTE.equals( eUnit ) )
    {
      final int seconds = multiplier * 60;
      return seconds;
    }
    else if( TimeStepUnitEnumStringType.SECOND.equals( eUnit ) )
      return multiplier;
    else
      throw new NotImplementedException();
  }
}
