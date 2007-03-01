package org.kalypso.kalypsomodel1d2d.conv;

/**
 * Provides a reversible id mapping von ascii 2d model to
 * gml:id.
 * The gml id are strings and the rma10s ids are positive non zero
 * integers
 *  
 * @author Patrice Congo
 */
public interface IModelElementIDProvider
{
   public int UNRESOLVED_NATIVE_ID=-1;
   
   /**
    * Gets the gml id for the provided native (rma10s) 2d element.
    * That element is specified by given the element type ky and
    * it native integer id 
    * @param elementKey the element key
    * @param rma10sID the native integer id
    * @return the gml id as string
    * @throws IllegalArgumentException if elementKey is null
    *           or the rma10sId negative or 0
    */
   public String rma10sToGmlID(
                       ERma10sModelElementKey elementKey, 
                       int rma10sID)
                       throws IllegalArgumentException;
   
   /**
    * Gets the rma10s id for the given gml id element.
    * @param elementKey the element key
    * @param the gml id
    * @return a positive non null integer if an rma10s id 
    *           corresponding to the given gml id is found
    *           otherwise {@link #UNRESOLVED_NATIVE_ID}
    * @throws IllegalArgumentException if elementKey is null
    *           or gmlID is null or an empty String 
    */
   public int gmlToRMA10S(
                       ERma10sModelElementKey elemenKey, 
                       String gmlID)
                       throws IllegalArgumentException;
   
   /**
    * Answer whether this provider accepts external id creations.
    * 
    * For example if the {@link #rma10sToGmlID(ERma10sModelElementKey, int)}
    * return null than the a feature is created and the id provider is
    * inform about the newly created id   
    * 
    * @return true if external ids are allows#
    * @see #inform(int, String)
    *  
    */
   public boolean allowExtenalID();
   
   /**
    * Inform the provider about an externaly (typically by the
    * workspace) created feature id
    * @param nativeID the nativeID
    */
   public void inform(
                 int nativeID, 
                 String gmlID)
                 throws IllegalArgumentException;
   
}