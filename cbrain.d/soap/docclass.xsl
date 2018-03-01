<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:php="http://php.net/xsl"
	xmlns:ipub="http://www.ipublisher.nl/4.0"
	xmlns:exsl="http://exslt.org/common"
	xmlns:str="http://exslt.org/strings"
	xmlns:date="http://exslt.org/dates-and-times"
	extension-element-prefixes="str exsl date"
	>
<xsl:include href="str.replace.function.xsl"/>
<xsl:output method="html" encoding="utf-8" indent="yes" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" media-type="text/html"/>

<xsl:template match="/model">
	<html>
	<head>
		<title>CBRain</title>
		<link rel="stylesheet" href="soap/doc.css" type="text/css" ></link>
	</head>
	<body>
	<div id="main">
	<div id="mainheader">
	<div id="mainheaderpadded">
		<xsl:if test="class != ''">
			<h1>
				<xsl:value-of select="class/name" />
				<a href="?class={class/name}&amp;wsdl">[WSDL]</a>
			</h1>
		</xsl:if>
	</div>
	</div>
	<div id="mainpadded">
	<table cellpadding="0" cellspacing="0">
	<tr>
	<td id="menu">
		<!-- <h2>Classes</h2> -->
		<xsl:for-each select="/model/menu/*">
			<a href="?class={name}"><xsl:value-of select="name"/></a><br />
		</xsl:for-each>
	</td>
	<td id="content">
			<xsl:if test="fault != ''">
				<xsl:value-of select="fault" />
			</xsl:if>
		<xsl:if test="class != '' and not(fault)">

			<xsl:if test="class/fullDescription != ''">
				<h2>Descrição da Classe</h2>
				<xsl:if test="class/version != ''">Revisão: <b><xsl:value-of select="class/version" /></b><br />
				</xsl:if>
				<xsl:if test="class/author != ''">Desenvolvedor: <b><xsl:value-of disable-output-escaping="yes" select="class/author" /></b><br />
				</xsl:if>
				<pre><xsl:value-of select="class/fullDescription" /></pre>
			</xsl:if>

			<xsl:if test="class/properties/*"><h2>Propriedades</h2></xsl:if>
			<div class="property{warning}">
			<xsl:for-each select="class/properties/*">
				<a name="property_{name}"></a>
				<span style="color:#5a95da"><xsl:value-of select="name" /></span>&#160;<xsl:choose>
					<xsl:when test="type != ''">
						<xsl:choose>
							<xsl:when test="contains('int,boolean,double,float,string,void', type)">
								<i><xsl:value-of select="type" /></i>
							</xsl:when>
							<xsl:otherwise>
								<i><a href="?class={str:replace(type,'[]','')}"><xsl:value-of select="type" /></a></i>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:when>
					<xsl:otherwise>
						<div class='warning'><img src='soap/warning.gif'/> missing type info</div>
					</xsl:otherwise>
				</xsl:choose>
				<xsl:if test="description != ''">:&#160;<xsl:value-of select="description" /></xsl:if>
				<br />
			</xsl:for-each>
			</div>
			<xsl:if test="class/methods/*"><h2>Métodos</h2></xsl:if>
			<xsl:for-each select="class/methods/*">
				<a name="method_{name}"></a>
				<div class="method{warning}">
				<xsl:if test="deprecated"><b style="color:silver; text-decoration: line-through"><xsl:value-of select="name" /></b></xsl:if>
				<xsl:if test="not(deprecated)"><b><xsl:value-of select="name" /></b></xsl:if>
				<br />
				<i>&#160;&#160;Params</i><br />
				<xsl:for-each select="params/*">
					&#160;&#160;&#160;&#160;<span style="color:#5a95da"><xsl:value-of select="name"/></span>&#160;<xsl:choose>
						<xsl:when test="contains('int,boolean,double,float,string,void', type)"><i><xsl:value-of select="type" /></i></xsl:when>
						<xsl:otherwise><i><a href="?class={str:replace(type,'[]','')}"><xsl:value-of select="type" /></a></i></xsl:otherwise>
					</xsl:choose>
					<xsl:if test="doc != ''">:&#160;<xsl:value-of select="doc" /></xsl:if>
					<br />
				</xsl:for-each>
				<xsl:choose>
					<xsl:when test="return != ''">
						<xsl:choose>
							<xsl:when test="contains('int,boolean,double,float,string,void', return/type)">
								&#160;&#160;&#160;&#160;<span style="color:#23599c">Returns</span>&#160;<i><xsl:value-of select="return/type" /></i>
							</xsl:when>
							<xsl:otherwise>
								&#160;&#160;&#160;&#160;<span style="color:#23599c">Returns</span>&#160;<i><a href="?class={str:replace(return/type,'[]','')}"><xsl:value-of select="return/type" /></a></i>
							</xsl:otherwise>
						</xsl:choose>
						<xsl:if test="return/doc != ''">:&#160;<xsl:value-of select="return/doc" /></xsl:if>
						<br />
					</xsl:when>
					<xsl:otherwise>
						<div class='warning'><img src='soap/warning.gif'/> missing return value</div><br />
					</xsl:otherwise>
				</xsl:choose>
				<xsl:choose>
					<xsl:when test="throws != ''">
						<i>&#160;&#160;Throws<br />&#160;&#160;&#160;&#160;<xsl:value-of select="throws" /></i><br />
					</xsl:when>
				</xsl:choose>
				<xsl:if test="fullDescription != ''"><i>&#160;&#160;Documentation</i><pre><xsl:value-of select="fullDescription" /></pre><br /></xsl:if>

				</div>
			</xsl:for-each>
		</xsl:if>
	</td>
	</tr>
	</table>

	</div>
	<div id="mainfooter"></div>
	</div>
	</body>
	</html>
</xsl:template>
</xsl:stylesheet>