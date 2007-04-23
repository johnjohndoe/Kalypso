/**
 * 
 */
package org.kalypso.afgui.scenarios;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public interface IScenarioManagerListener
{
  public void scenarioAdded( final Scenario scenario );

  public void scenarioRemoved( final Scenario scenario );
}
