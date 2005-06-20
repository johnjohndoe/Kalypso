package org.kalypsodeegree_impl.extension;

import java.net.URL;

import org.kalypso.java.net.IUrlResolver;
import org.w3c.dom.Node;

/**
 * @author belger
 */
public interface ITypeHandler
{
  /** Der Java-Name, auf den dieser Typ gemapt wird */
  public String getClassName();

  /** Der XML-Typname */
  public String getTypeName();

  /**
   * Serialize object to xml node
   * 
   * @param node
   *          serialize the object into this node
   * @param context
   *          use this context for relative url
   * @param object
   *          object to serialize, it must be instanceof {@link #getClassName()}.
   */
  public void marshall( final Object object, final Node node, URL context ) throws TypeRegistryException;

  /**
   * creates an object of type {@link #getClassName()}from node.
   * 
   * @param node
   *          unmnarshall this node to an object of type {@link #getClassName()}
   * @param context
   *          use this context for relative url
   */
  public Object unmarshall( final Node node, URL context, IUrlResolver urlResolver ) throws TypeRegistryException;

  /** Ein Kurzname des behandelten Typ, wird z.B: für Beschriftungen benutzt */
  public String getShortname();
}