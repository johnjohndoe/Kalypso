package org.deegree_impl.extension;

import java.util.HashMap;
import java.util.Map;

/**
 * Standardimplementation von {@link org.deegree_impl.extension.ITypeRegistry}.
 * 
 * @author belger
 */
public class TypeRegistry_impl implements ITypeRegistry
{
  /** typeName -> handler */
  private Map m_typeMap = new HashMap();

  /** className -> handler */
  private Map m_classMap = new HashMap();

  /**
   * Falls TypeName oder ClassName bereits belegt sind
   * @throws TypeRegistryException
   * 
   * @see org.deegree_impl.extension.ITypeRegistry#registerTypeHandler(org.deegree_impl.extension.ITypeHandler)
   */
  public void registerTypeHandler( final ITypeHandler typeHandler ) throws TypeRegistryException
  {
    final String typeName = typeHandler.getTypeName();
    final String className = typeHandler.getClassName();

    if( m_typeMap.containsKey( typeName ) )
      throw new TypeRegistryException( "Typname wurde bereits registriert: " + typeName );

    if( m_classMap.containsKey( className ) )
      throw new TypeRegistryException( "Classname wurde bereits registriert: " + className );
    
    m_typeMap.put( typeName, typeHandler );
    m_classMap.put( className, typeHandler );
  }

  /**
   * @throws TypeRegistryException
   * @see org.deegree_impl.extension.ITypeRegistry#getTypeHandlerForTypeName(java.lang.String)
   */
  public ITypeHandler getTypeHandlerForTypeName( final String typeName ) throws TypeRegistryException
  {
    if( !m_typeMap.containsKey( typeName ) )
      throw new TypeRegistryException( "Typname nicht bekannt: " + typeName );
    
    return (ITypeHandler)m_typeMap.get( typeName );
  }

  /**
   * @throws TypeRegistryException falls className nicht bekannt ist
   * @see org.deegree_impl.extension.ITypeRegistry#getTypeHandlerForClassName(java.lang.String)
   */
  public ITypeHandler getTypeHandlerForClassName( final String className ) throws TypeRegistryException
  {
    if( !m_classMap.containsKey( className ) )
      throw new TypeRegistryException( "Classname nicht bekannt: " + className );
    
    return (ITypeHandler)m_classMap.get( className );
  }

  /**
   * @see org.deegree_impl.extension.ITypeRegistry#unregisterTypeHandler(org.deegree_impl.extension.ITypeHandler)
   */
  public void unregisterTypeHandler( ITypeHandler typeHandler )
  {
    m_typeMap.remove( typeHandler.getTypeName() );
    m_classMap.remove(typeHandler.getClassName() );
  }
}
