package org.deegree_impl.extension;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class GMLBoundingShapeTypeHandler implements ITypeHandler
{

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return GM_Envelope.class.getName().toString();
  }

  private static String m_nameSpaceURI=XMLHelper.GMLSCHEMA_NS+":"+"BoundingShapeType";
  /**
   * @see org.deegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return m_nameSpaceURI;
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#marshall(java.lang.Object, org.w3c.dom.Node)
   */
  public void marshall( Object object, Node node ) throws TypeRegistryException
  {
  throw new UnsupportedOperationException("TODO implement it");  
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node)
   */
  public Object unmarshall( Node node ) throws TypeRegistryException
  {
  throw new UnsupportedOperationException("TODO implement it");  
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Bounding Shape";
  }

}
