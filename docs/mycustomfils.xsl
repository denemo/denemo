<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format" version="1.0"> 
  <xsl:import href="/usr/share/sgml/docbook/xsl-stylesheets-1.68.1/fo/docbook.xsl"/> 
    <xsl:param name="paper.type" select="'A4'"/>
    <xsl:param name="body.font.master">10</xsl:param>
<!-- <xsl:param name="page.margin.inner">0.10in</xsl:param>
<xsl:param name="page.margin.outer">0.50in</xsl:param>-->
    
<xsl:param name="page.margin.inner">0.75in</xsl:param>
<xsl:param name="page.margin.outer">0.75in</xsl:param>
<xsl:param name= "page.margin.top">0.55in</xsl:param>   
<xsl:param name="region.before.extent">0.75in</xsl:param>  
<xsl:param name="body.margin.top">1.75in</xsl:param>  
<xsl:param name="region.after.extent">0.55in</xsl:param>
<xsl:param name="page.margin.bottom">0.55in</xsl:param>
<xsl:param name="body.margin.bottom">0.55in</xsl:param>
<xsl:param name="title.margin.left">0.025pt</xsl:param>
    <xsl:param name="xep.extensions">1</xsl:param>
    <xsl:param name="draft.mode">yes</xsl:param>
    <xsl:param name="admon.graphics">1</xsl:param>
    <xsl:param name="admon.textlabel">1</xsl:param> 

<xsl:param name="generate.toc">

book  nop

</xsl:param>
<!--
<xsl:template match="graphicco">
<xsl:apply-templates select="graphic"/>
</xsl:template>

<xsl:template match="graphicco/graphic">
<xsl:variable name="id" select="generate-id()"/>
<img border="0" usemap="#{$id}" src="{concat($img.src.path, @fileref)}">
  <map name="{$id}">
    <xsl:apply-templates select="../areaspec/area"/>
  </map>
</img>
</xsl:template>
-->
<xsl:template match="graphicco/areaspec/area">
<area href="#{@linkends}" shape="polygon" coords="{@coords}"/>
</xsl:template>

<xsl:template match="graphicco/areaspec"/>


<xsl:template name="book.titlepage.separator">
</xsl:template>




<!-- Begin customization for header -->
<xsl:template name="header.content">
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="position" select="''"/>
  <xsl:param name="gentext-key" select="''"/>

<!--
  <fo:block>
    <xsl:value-of select="$pageclass"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$sequence"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$position"/>
    <xsl:text>, </xsl:text>
    <xsl:value-of select="$gentext-key"/>
  </fo:block>
-->

 
  <fo:block>

    <!-- sequence can be odd, even, first, blank -->
    <!-- position can be left, center, right -->
    <xsl:choose>
      <xsl:when test="$sequence = 'blank'">
        <!-- nothing -->
      </xsl:when>

      <xsl:when test="$position='center'">
        <!-- Same for odd, even, empty, and blank sequences -->
	<fo:external-graphic src="/demudi/denemo/doc/images/logo1.png" content-width="1.77in" content-height="0.472in"  padding-bottom=".75cm" scaling="uniform"/>
        
      </xsl:when>

      <xsl:when test="($sequence='odd' or $sequence='even') and $position='left'">
        <xsl:if test="$pageclass != 'titlepage'">
          <xsl:choose>
            <xsl:when test="ancestor::book and ($double.sided != 0)">
              <fo:retrieve-marker retrieve-class-name="section.head.marker"
                                  retrieve-position="first-including-carryover"
                                  retrieve-boundary="page-sequence"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates select="." mode="titleabbrev.markup"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>
      </xsl:when>

      <xsl:when test="$position='center'">
        <!-- Same for odd, even, empty, and blank sequences -->
        
      </xsl:when>

      <xsl:when test="$position='right'">
        <!-- Same for odd, even, empty, and blank sequences -->
        <xsl:call-template name="draft.text"/>
      </xsl:when>

    </xsl:choose>

  </fo:block>
</xsl:template>

<!-- End customization for header -->

<xsl:attribute-set name="component.title.properties">
  <xsl:attribute name="space-before.minimum">1.15em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">1.20em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">1.30em</xsl:attribute>
<xsl:attribute name="space-after.optimum">.110em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">1.20m</xsl:attribute>
  <xsl:attribute name="space-after.maximum">1.36em</xsl:attribute>
  <!--<xsl:attribute name="start-indent">2.0pt</xsl:attribute>-->
</xsl:attribute-set>

<xsl:attribute-set name="section.title.level1.properties">
   <xsl:attribute name="font-size">12pt</xsl:attribute>
   <xsl:attribute name="space-before.minimum">0.15em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">.25em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">.30em</xsl:attribute>
<!--<xsl:attribute name="start-indent">0pt</xsl:attribute>-->
   </xsl:attribute-set>


<xsl:attribute-set name="section.title.level2.properties">
   <xsl:attribute name="font-size">11pt</xsl:attribute>
   <xsl:attribute name="space-before.minimum">0.15em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">.20em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">.30em</xsl:attribute>
<xsl:attribute name="space-after.optimum">.60em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.40m</xsl:attribute>
  <xsl:attribute name="space-after.maximum">.56em</xsl:attribute>
<!--<xsl:attribute name="start-indent">0pt</xsl:attribute>-->
   </xsl:attribute-set>

<xsl:attribute-set name="section.title.level3.properties">
 <xsl:attribute name="font-size">9.5pt</xsl:attribute>
   <xsl:attribute name="space-before.minimum">0.05em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">.10em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">.20em</xsl:attribute>
<xsl:attribute name="space-after.optimum">.60em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.40m</xsl:attribute>
  <xsl:attribute name="space-after.maximum">.56em</xsl:attribute>
<!--<<xsl:attribute name="start-indent">0pt</xsl:attribute>-->
</xsl:attribute-set>


<xsl:attribute-set name="normal.para.spacing">
  <xsl:attribute name="space-before.optimum">.3em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.14em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.4em</xsl:attribute>
 <xsl:attribute name="space-after.optimum">.50em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.30m</xsl:attribute>
  <xsl:attribute name="space-after.maximum">.46em</xsl:attribute>

</xsl:attribute-set>

<xsl:attribute-set name="admonition.title.properties">
<xsl:attribute name="space-before.optimum">.3em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.14em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.4em</xsl:attribute>
<xsl:attribute name="space-after.optimum">.20em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.10m</xsl:attribute>
  <xsl:attribute name="space-after.maximum">.36em</xsl:attribute>
  <xsl:attribute name="font-size">10pt</xsl:attribute>
  <xsl:attribute name="font-weight">bold</xsl:attribute>
  <xsl:attribute name="hyphenate">false</xsl:attribute>
  <xsl:attribute name="keep-with-next.within-column">always</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="list.block.spacing">
 <xsl:attribute name="margin.left">.65em</xsl:attribute>
  <xsl:attribute name="space-before.optimum">.40em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.3em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.6em</xsl:attribute>
  <xsl:attribute name="space-after.optimum">.60em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.40m</xsl:attribute>
  <xsl:attribute name="space-after.maximum">.56em</xsl:attribute>
</xsl:attribute-set>
<xsl:attribute-set name="list.item.spacing">
  <xsl:attribute name="space-before.optimum">.10em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.14em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.30em</xsl:attribute>
</xsl:attribute-set>
<!-- Begin customization for footer -->
</xsl:stylesheet> 
