package org.kalypso.util.command;

import java.util.EventListener;

/**
 * @author belger
 */
public interface ICommandManagerListener extends EventListener
{
  public void onCommandManagerChanged( final ICommandManager source );
}
