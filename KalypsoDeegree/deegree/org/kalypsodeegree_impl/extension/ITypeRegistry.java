package org.deegree_impl.extension;

/**
 * Die Type - Registry dient dazu, neue Typen im System anzumelden und
 * die entsprechende Handler zu finden. 
 * 
 * @author belger
 */
public interface ITypeRegistry
{
  public void registerTypeHandler( final ITypeHandler typeHandler ) throws TypeRegistryException;
  
  public void unregisterTypeHandler( final ITypeHandler typeHandler );
  
  public ITypeHandler getTypeHandlerForTypeName( final String typeName ) throws TypeRegistryException;;
  
  public ITypeHandler getTypeHandlerForClassName( final String className ) throws TypeRegistryException;
}
