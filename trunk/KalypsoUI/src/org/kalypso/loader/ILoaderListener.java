package org.kalypso.loader;

/**
 * @author belger
 */
public interface ILoaderListener
{
  /** Wird aufgerufen, wenn dieses Objekt des Loaders nicht mehr gültig ist und neu geladen werden sollte */
  public void onLoaderObjectInvalid( final Object object, final boolean bCannotReload ) throws Exception;
}
