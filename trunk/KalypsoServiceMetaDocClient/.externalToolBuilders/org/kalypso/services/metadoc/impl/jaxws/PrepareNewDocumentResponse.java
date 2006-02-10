
package org.kalypso.services.metadoc.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.metadoc.impl.jaxws.PrepareNewDocumentResponse;

@XmlRootElement(name = "prepareNewDocumentResponse", namespace = "http://impl.metadoc.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "prepareNewDocumentResponse", namespace = "http://impl.metadoc.services.kalypso.org/")
public class PrepareNewDocumentResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.services.metadoc.PrepareBean _return;

    /**
     * 
     * @return
     *     returns PrepareBean
     */
    public org.kalypso.services.metadoc.PrepareBean get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.services.metadoc.PrepareBean _return) {
        this._return = _return;
    }

}
