//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.0-b26-ea3 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.09.30 at 03:28:54 PM CEST 
//


package com.google.earth.kml;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.google.earth.kml.ItemIconType;
import com.google.earth.kml.ListItemTypeEnum;
import com.google.earth.kml.ListStyleType;
import com.google.earth.kml.ObjectType;


/**
 * <p>Java class for ListStyleType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ListStyleType">
 *   &lt;complexContent>
 *     &lt;extension base="{http://earth.google.com/kml/2.1}ObjectType">
 *       &lt;sequence>
 *         &lt;element name="listItemType" type="{http://earth.google.com/kml/2.1}listItemTypeEnum" minOccurs="0"/>
 *         &lt;element name="bgColor" type="{http://earth.google.com/kml/2.1}color" minOccurs="0"/>
 *         &lt;element name="ItemIcon" type="{http://earth.google.com/kml/2.1}ItemIconType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "ListStyleType", propOrder = {
    "listItemType",
    "bgColor",
    "itemIcon"
})
public class ListStyleType
    extends ObjectType
{

    @XmlElement(namespace = "http://earth.google.com/kml/2.1", defaultValue = "check")
    protected ListItemTypeEnum listItemType;
    @XmlElement(namespace = "http://earth.google.com/kml/2.1", type = String.class, defaultValue = "ffffffff")
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    protected byte[] bgColor;
    @XmlElement(name = "ItemIcon", namespace = "http://earth.google.com/kml/2.1")
    protected List<ItemIconType> itemIcon;

    /**
     * Gets the value of the listItemType property.
     * 
     * @return
     *     possible object is
     *     {@link ListItemTypeEnum }
     *     
     */
    public ListItemTypeEnum getListItemType() {
        return listItemType;
    }

    /**
     * Sets the value of the listItemType property.
     * 
     * @param value
     *     allowed object is
     *     {@link ListItemTypeEnum }
     *     
     */
    public void setListItemType(ListItemTypeEnum value) {
        this.listItemType = value;
    }

    /**
     * Gets the value of the bgColor property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getBgColor() {
        return bgColor;
    }

    /**
     * Sets the value of the bgColor property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setBgColor(byte[] value) {
        this.bgColor = ((byte[]) value);
    }

    /**
     * Gets the value of the itemIcon property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the itemIcon property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getItemIcon().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ItemIconType }
     * 
     * 
     */
    public List<ItemIconType> getItemIcon() {
        if (itemIcon == null) {
            itemIcon = new ArrayList<ItemIconType>();
        }
        return this.itemIcon;
    }

}
