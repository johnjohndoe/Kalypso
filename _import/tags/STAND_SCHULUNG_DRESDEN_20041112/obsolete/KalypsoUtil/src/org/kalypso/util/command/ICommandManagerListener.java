package org.kalypso.util.command;

import java.util.EventListener;

/**
 * <p>Dieses Interface makriert Observer auf einem {@link ICommandManager}.</p>
 * <p>Die Listener werden über Änderungen des Managers informiert</p>
 *
 * @author belger
 */
public interface ICommandManagerListener extends EventListener
{
  public void onCommandManagerChanged( final ICommandManager source );
}
