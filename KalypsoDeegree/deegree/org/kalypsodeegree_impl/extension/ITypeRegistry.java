package org.deegree_impl.extension;

/**
 * Die Type - Registry dient dazu, neue Typen im System anzumelden und die
 * entsprechende Handler zu finden.
 * 
 * @author belger
 */
public interface ITypeRegistry
{
  public void registerTypeHandler( final ITypeHandler typeHandler ) throws TypeRegistryException;

  public void unregisterTypeHandler( final ITypeHandler typeHandler );

  public boolean hasTypeName( final String typeName );

  public boolean hasClassName( final String className );

  public ITypeHandler getTypeHandlerForTypeName( final String typeName );

  public ITypeHandler getTypeHandlerForClassName( final String className );
}