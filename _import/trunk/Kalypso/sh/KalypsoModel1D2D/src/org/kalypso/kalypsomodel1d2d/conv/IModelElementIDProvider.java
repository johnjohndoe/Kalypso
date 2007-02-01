package org.kalypso.kalypsomodel1d2d.conv;

/**
 * Provides a reversible id mapping von ascii 2d model to
 * gml:id 
 * @author Patrice Congo
 *
 */
public interface IModelElementIDProvider
{
   /**
    * Gets the gml id for the provided bc 2d element
    * @param elementKey the element key
    */
   public String rma10sToGmlID(
                         ERma10sModelElementKey elementKey, 
                         int rma10sID);
   
   /**
    * Gets the rma10s id for the given gml id element
    * @param elementKey the element key
    * @param 
    */
   public int gmlToRMA10S(
                         ERma10sModelElementKey elemenKey, 
                         int element);
}