package org.kalypso.util.adapter;

/**
 * An interface for an adaptable object.
 * <p>
 * Adaptable objects can be dynamically extended to provide different interfaces
 * (or "adapters").
 * <p>
 * For example,
 * 
 * <pre>
 * IAdaptable a = [some adaptable];
 * IFoo x = (IFoo)a.getAdapter(IFoo.class);
 * if (x != null)
 *  [do IFoo things with x]
 * </pre>
 * 
 * <p>
 * Inspired by the IAdaptable interface from the Eclipse core package.
 * 
 * @author schlienger 
 */
public interface IAdaptable
{
  public Object getAdapter( final Class anotherClass );
}