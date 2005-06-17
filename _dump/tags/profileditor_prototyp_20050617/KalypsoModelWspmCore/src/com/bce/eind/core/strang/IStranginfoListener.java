package com.bce.eind.core.strang;


/**
 * @author gernot
 */
public interface IStranginfoListener
{
  /** Called, if someone tries to change the index, return false, if you don't want to */
  public boolean onTryChangeIndex( final StrangInfo source );
  
  public void onIndexChanged( final StrangInfo source );
}
