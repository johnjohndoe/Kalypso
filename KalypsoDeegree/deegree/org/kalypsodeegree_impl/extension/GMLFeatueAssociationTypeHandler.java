package org.kalypsodeegree_impl.extension;

import java.net.URL;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Node;

/**
 * @author doemming
 */
public class GMLFeatueAssociationTypeHandler implements ITypeHandler
{

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return Feature.class.getName().toString();
  }

  private static String m_nameSpaceURI = XMLHelper.GMLSCHEMA_NS + ":" + "FeatureAssociationType";

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return m_nameSpaceURI;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#marshall(java.lang.Object, org.w3c.dom.Node, java.net.URL)
   */
  public void marshall( Object object, Node node,URL context  )
  {
    throw new UnsupportedOperationException( "TODO implement it" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL)
   */
  public Object unmarshall( Node node,URL context  )
  {
    throw new UnsupportedOperationException( "TODO implement it" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Feature link";
  }

}