package org.kalypso.util.command;

/**
 * Helper-Interface f�r Command -Zeugs
 * 
 * @author belger
 */
public interface ICommandPoster
{
  public void postCommand( final ICommand command, final Runnable runnable );
}
